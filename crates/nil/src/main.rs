use anyhow::{ensure, Context, Result};
use argh::FromArgs;
use codespan_reporting::term::termcolor::WriteColor;
use ide::{AnalysisHost, Change, FileId, FileSet, Severity, SourceRoot, VfsPath};
use std::io::IsTerminal;
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
    /// use stdin and stdout for the language server. This is the default but it suppress
    /// warnings when either stdin or stdout is tty.
    #[argh(switch)]
    stdio: bool,
    #[argh(subcommand)]
    subcommand: Option<Subcommand>,
}

#[derive(Debug, FromArgs)]
#[argh(subcommand)]
enum Subcommand {
    Diagnostics(DiagnosticsArgs),
    Parse(ParseArgs),
    Ssr(SsrArgs),
}

#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "diagnostics")]
/// Check and print diagnostics for a file.
/// Output will be empty if there are no errors/warnigns.
/// Exit with non-zero code if there are any errors. (`0` for only warnings)
/// WARNING: The output format is for humans and should not be relied on.
struct DiagnosticsArgs {
    /// treat warnings like errors and exit with non-zero code.
    #[argh(switch)]
    deny_warnings: bool,

    /// nix files to check, or read from stdin for `-`.
    /// NB. You need `--` before `-` for paths starting with `-`,
    /// to disambiguous it from flags.
    #[argh(positional)]
    paths: Vec<PathBuf>,
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
            Subcommand::Ssr(args) => main_ssr(args),
        };
    }

    setup_logger();

    if !args.stdio && (io::stdin().is_terminal() || io::stdout().is_terminal()) {
        // TODO: Make this a hard error.
        eprintln!(
            "\
            WARNING: You are NOT supposed to run the language server in terminal. This will be a \
            hard error in the future. Please configure it in your editor instead.\
            "
        );
    }

    let ret = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("Failed to spawn tokio runtime")
        .block_on(nil::run_server_stdio());
    match ret {
        Ok(()) => {}
        Err(err) => {
            tracing::error!("Unexpected error: {err:#}");
            process::exit(101);
        }
    }
}

fn main_diagnostics(args: DiagnosticsArgs) {
    let ret = emit_diagnostics_for_files(&args.paths);

    let fail_threshould = if args.deny_warnings {
        Severity::Warning
    } else {
        Severity::Error
    };

    match ret {
        Ok(Some(severity)) if severity >= fail_threshould => process::exit(1),
        Ok(_) => {}
        Err(err) => {
            eprintln!("{err:#}");
            process::exit(1);
        }
    }
}

fn emit_diagnostics_for_files(paths: &[PathBuf]) -> Result<Option<ide::Severity>> {
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    ensure!(!paths.is_empty(), "no file is provided");

    let mut sources = Vec::new();
    let mut file_set = FileSet::default();
    let mut change = Change::default();
    for (id, path) in paths.iter().enumerate() {
        let file = FileId(id as u32);

        let src = if path.as_os_str() == "-" {
            io::read_to_string(io::stdin().lock()).context("Failed to read from stdin")?
        } else {
            fs::read_to_string(path).context("Failed to read file")?
        };

        let src: Arc<str> = src.into();
        sources.push((file, path, src.clone()));

        change.change_file(file, src);
        file_set.insert(file, VfsPath::new(path));
    }

    change.set_roots(vec![SourceRoot::new_local(file_set, None)]);

    let mut analysis = AnalysisHost::new();
    analysis.apply_change(change);

    let snapshot = analysis.snapshot();

    let mut writer = StandardStream::stdout(ColorChoice::Auto);
    let mut max_severity = None;
    for (id, path, src) in sources {
        let diagnostics = snapshot.diagnostics(id).expect("No cancellation");
        emit_diagnostics(path, &src, &mut writer, &mut diagnostics.iter().cloned())?;

        // Note: None < Some(_)
        max_severity = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.severity())
            .max()
            .max(max_severity);
    }

    Ok(max_severity)
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

#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "ssr")]
/// Search structural patterns and optionally replace them.
/// WARNING: This functionality is experimental.
struct SsrArgs {
    /// nix file to check, or read from stdin for `-`.
    /// NB. You need `--` before `-` for paths starting with `-`,
    /// to disambiguous it from flags.
    #[argh(positional)]
    path: PathBuf,
    /// expression pattern to search. Placeholders `$name` can be used to capture sub-expressions.
    #[argh(positional)]
    pattern: String,
    /// expression template to replace. Placeholders `$name` can be used to substitute
    /// sub-expressions from the pattern.
    #[argh(positional)]
    template: Option<String>,
}

fn main_ssr(args: SsrArgs) {
    let ret = (|| -> Result<()> {
        let path = &*args.path;

        let src = if path.as_os_str() == "-" {
            io::read_to_string(io::stdin().lock()).context("Failed to read from stdin")?
        } else {
            fs::read_to_string(path).context("Failed to read file")?
        };

        let parse = syntax::parse_file(&src);
        let pat = ssr::Pattern::parse(&args.pattern).context("invalid SSR pattern")?;

        match args.template {
            None => {
                for n in pat.find_iter(&parse.syntax_node()) {
                    let range = n.text_range();
                    println!(
                        "{}:{}-{}:{}",
                        args.path.display(),
                        u32::from(range.start()),
                        u32::from(range.end()),
                        &src[range],
                    );
                }
            }
            Some(templ) => {
                let templ = ssr::Template::parse(&templ, &pat).context("invalid SSR template")?;
                let ret = pat.replace(&src, &templ, &parse.syntax_node());
                print!("{ret}");
            }
        }

        Ok(())
    })();

    match ret {
        Ok(()) => {}
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
        let labels = std::iter::once(Label::primary(cr_file, to_range(diag.range))).chain(
            diag.notes.iter().map(|(frange, note)| {
                Label::secondary(cr_file, to_range(frange.range)).with_message(note)
            }),
        );
        let diag = Diagnostic::new(severity)
            .with_code(diag.code())
            .with_message(diag.message())
            .with_labels_iter(labels);

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
