use std::path::PathBuf;

use criterion::{BatchSize, Criterion};
use once_cell::sync::Lazy;

const TEST_FILES: &[(&str, &str)] = &[
    ("gcc-13", "pkgs/development/compilers/gcc/default.nix"),
    ("all-packages", "pkgs/top-level/all-packages.nix"),
];

static NIXPKGS: Lazy<PathBuf> = Lazy::new(|| {
    std::env::var("NIXPKGS")
        .expect("missing env var NIXPKGS")
        .into()
});

fn bench_parser(c: &mut Criterion) {
    for &(name, path) in TEST_FILES {
        let path = NIXPKGS.join(path);
        let src = std::fs::read_to_string(path).expect("failed to read test file");

        c.bench_function(&format!("lex_{name}"), |b| {
            b.iter(|| syntax::lexer::lex(src.as_bytes()));
        });

        c.bench_function(&format!("parse_{name}"), |b| {
            let toks = syntax::lexer::lex(src.as_bytes());
            b.iter_batched(
                || toks.clone(),
                |toks| syntax::parser::parse_file_tokens(&src, toks),
                BatchSize::SmallInput,
            );
        });
    }
}

#[cfg(not(debug_assertions))]
fn bench_nixpkgs(c: &mut Criterion) {
    c.bench_function("whole_nixpkgs", |b| {
        let srcs = walkdir::WalkDir::new(&**NIXPKGS)
            .follow_links(false)
            .sort_by_file_name()
            .same_file_system(true)
            .into_iter()
            .filter_map(|ent| {
                let ent = ent.expect("failed to traverse NIXPKGS");
                if !ent.file_type().is_file()
                    || ent.path().extension().map_or(true, |ext| ext != "nix")
                {
                    return None;
                }
                let src = std::fs::read_to_string(ent.path()).expect("failed to read file");
                Some(src)
            })
            .collect::<Vec<_>>();
        // Sanity check.
        assert!(srcs.len() > 1000);

        b.iter_batched(
            || {},
            |()| {
                srcs.iter()
                    .map(|src| syntax::parse_file(src))
                    .collect::<Vec<_>>()
            },
            BatchSize::PerIteration,
        );
    });
}

#[cfg(debug_assertions)]
criterion::criterion_group!(benches, bench_parser);

#[cfg(not(debug_assertions))]
criterion::criterion_group!(benches, bench_parser, bench_nixpkgs);

criterion::criterion_main!(benches);
