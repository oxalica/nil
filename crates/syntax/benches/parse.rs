use std::path::PathBuf;

use criterion::Criterion;

const TEST_FILES: &[(&str, &str)] = &[
    ("gcc-13", "pkgs/development/compilers/gcc/13/default.nix"),
    ("all-packages", "pkgs/top-level/all-packages.nix"),
];

fn bench_parser(c: &mut Criterion) {
    let nixpkgs_path = PathBuf::from(std::env::var("NIXPKGS").expect("missing env var NIXPKGS"));

    for &(name, path) in TEST_FILES {
        let path = nixpkgs_path.join(path);
        let src = std::fs::read_to_string(path).expect("failed to read test file");

        c.bench_function(&format!("lex_{name}"), |b| {
            b.iter(|| syntax::lexer::lex(src.as_bytes()));
        });

        c.bench_function(&format!("parse_{name}"), |b| {
            let toks = syntax::lexer::lex(src.as_bytes());
            b.iter_batched(
                || toks.clone(),
                |toks| syntax::parser::parse_file_tokens(&src, toks),
                criterion::BatchSize::SmallInput,
            );
        });
    }
}

criterion::criterion_group!(benches, bench_parser);
criterion::criterion_main!(benches);
