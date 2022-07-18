mod def;
mod source;

#[cfg(test)]
mod tests;

use crate::{def::DefDatabaseStorage, source::SourceDatabaseStorage};

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}
