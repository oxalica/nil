use anyhow::{Context, Result};
use argh::FromArgs;
use codespan_reporting::term::termcolor::WriteColor;
use ide::AnalysisHost;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{env, fs, io, process};
use text_size::TextRange;
use tracing_subscriber::fmt::writer::BoxMakeWriter;
use tracing_subscriber::EnvFilter;

const LOG_FILTER_ENV: &str = "NIL_LOG";
const LOG_PATH_ENV: &str = "NIL_LOG_PATH";
const BACKTRACE_ENV: &str = "RUST_BACKTRACE";

#[derive(Debug, FromArgs)]
/// LSP server for Nix Expression Language.
/// Run without arguments to start the language server on stdin/stdout.
struct Args {
    /// print the version and exit
    #[argh(switch)]
    version: bool,
    #[argh(subcommand)]
    subcommand: Option<Subcommand>,
}

#[derive(Debug, FromArgs)]
#[argh(subcommand)]
enum Subcommand {
    Diagnostics(DiagnosticsArgs),
    Parse(ParseArgs),
}

#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "diagnostics")]
/// Check and print diagnostics for a file.
/// Exit with non-zero code if there are any diagnostics.
/// WARNING: The output format is for human and should not be relied on.
struct DiagnosticsArgs {
    /// nix file to check, or read from stdin for `-`.
    /// NB. You need `--` before `-` for paths starting with `-`,
    /// to disambiguous it from flags.
    #[argh(positional)]
    path: PathBuf,
}

#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "parse")]
/// Parse a Nix file, print syntax tree in stdout and parse errors in stderr.
/// Exit with non-zero code if there are any errors.
/// WARNING: The output, including the syntax tree layout and error format, are for human and
/// should not be relied on.
struct ParseArgs {
    /// nix file to check, or read from stdin for `-`.
    /// NB. You need `--` before `-` for paths starting with `-`,
    /// to disambiguous it from flags.
    #[argh(positional)]
    path: PathBuf,
}

fn main() {
    if env::var(BACKTRACE_ENV).is_err() {
        env::set_var(BACKTRACE_ENV, "short");
    }

    let args = argh::from_env::<Args>();
    if args.version {
        let release = option_env!("CFG_RELEASE").unwrap_or("unknown");
        println!("nil {release}");
        return;
    }

    if let Some(subcommand) = args.subcommand {
        return match subcommand {
            Subcommand::Diagnostics(args) => main_diagnostics(args),
            Subcommand::Parse(args) => main_parse(args),
        };
    }

    setup_logger();

    match nil::run_server_stdio() {
        Ok(()) => {}
        Err(err) => {
            tracing::error!("Unexpected error: {err:#}");
            process::exit(101);
        }
    }
}

fn main_diagnostics(args: DiagnosticsArgs) {
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    let ret = (|| -> Result<bool> {
        let path = &*args.path;

        let src = if path.as_os_str() == "-" {
            io::read_to_string(io::stdin().lock()).context("Failed to read from stdin")?
        } else {
            fs::read_to_string(path).context("Failed to read file")?
        };

        let (analysis, file) = AnalysisHost::new_single_file(&src);
        let diags = analysis
            .snapshot()
            .diagnostics(file)
            .expect("No cancellation");

        let mut writer = StandardStream::stdout(ColorChoice::Auto);
        emit_diagnostics(path, &src, &mut writer, &mut diags.iter().cloned())?;
        Ok(diags.is_empty())
    })();
    match ret {
        Ok(true) => {}
        Ok(false) => process::exit(1),
        Err(err) => {
            eprintln!("{err:#}");
            process::exit(1);
        }
    }
}

fn main_parse(args: ParseArgs) {
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    let ret = (|| -> anyhow::Result<bool> {
        let path = &*args.path;

        let src = if path.as_os_str() == "-" {
            io::read_to_string(io::stdin().lock()).context("Failed to read from stdin")?
        } else {
            fs::read_to_string(path).context("Failed to read file")?
        };

        let parse = syntax::parse_file(&src);

        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        emit_diagnostics(
            path,
            &src,
            &mut writer,
            &mut parse.errors().iter().map(|&err| err.into()),
        )?;

        println!("{:#?}", parse.syntax_node());

        Ok(parse.errors().is_empty())
    })();
    match ret {
        Ok(true) => {}
        Ok(false) => process::exit(1),
        Err(err) => {
            eprintln!("{err:#}");
            process::exit(1);
        }
    }
}

fn emit_diagnostics(
    path: &Path,
    src: &str,
    writer: &mut dyn WriteColor,
    diags: &mut dyn Iterator<Item = ide::Diagnostic>,
) -> Result<()> {
    use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
    use codespan_reporting::files::SimpleFiles;
    use codespan_reporting::term;

    let config = codespan_reporting::term::Config::default();
    let to_range = |range: TextRange| usize::from(range.start())..usize::from(range.end());

    let mut files = SimpleFiles::new();
    let cr_file = files.add(path.display().to_string(), src);

    for diag in diags {
        let severity = match diag.severity() {
            ide::Severity::IncompleteSyntax | ide::Severity::Error => Severity::Error,
            ide::Severity::Warning => Severity::Warning,
        };
        let labels = std::iter::once(Label::primary(cr_file, to_range(diag.range)))
            .chain(diag.notes.iter().map(|(frange, note)| {
                Label::secondary(cr_file, to_range(frange.range)).with_message(note)
            }))
            .collect();
        let diag = Diagnostic::new(severity)
            .with_code(diag.code())
            .with_message(diag.message())
            .with_labels(labels);

        term::emit(writer, &config, &files, &diag)?;
    }

    Ok(())
}

fn setup_logger() {
    let file = env::var_os(LOG_PATH_ENV).and_then(|path| {
        let path = PathBuf::from(path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).ok()?;
        }
        fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .ok()
    });

    let writer = match file {
        Some(file) => BoxMakeWriter::new(Arc::new(file)),
        None => BoxMakeWriter::new(io::stderr),
    };

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_env(LOG_FILTER_ENV))
        .with_writer(writer)
        .init();
}
