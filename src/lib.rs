mod base;
mod def;
mod ide;

#[cfg(test)]
mod tests;

pub use base::{Change, FileId};
pub use ide::RootDatabase;
