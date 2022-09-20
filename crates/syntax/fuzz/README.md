To run fuzzer:
1. Enter dev-shell `fuzz` of top-level `flake.nix`
2. `cd` to this directory
3. Run `cargo fuzz run -j <NUM_THREADS> parser`
