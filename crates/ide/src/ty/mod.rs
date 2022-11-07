mod fmt;
mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use crate::{DefDatabase, FileId};
use std::sync::Arc;

pub use fmt::TyDisplay;
pub use infer::{Attrset, InferenceResult, Ty, TyKind};

#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase {
    #[salsa::invoke(infer::infer_query)]
    fn infer(&self, file: FileId) -> Arc<InferenceResult>;
}
