//! Nix defined file structures and interoperation with Nix.
pub mod eval;
pub mod flake_lock;

pub const DEFAULT_IMPORT_FILE: &str = "default.nix";
pub const FLAKE_FILE: &str = "flake.nix";
pub const FLAKE_LOCK_FILE: &str = "flake.lock";
