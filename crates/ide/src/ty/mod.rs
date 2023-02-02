#[rustfmt::skip]
#[macro_export]
macro_rules! ty {
    (?) => { $crate::ty::Ty::Unknown };
    (bool) => { $crate::ty::Ty::Int };
    (int) => { $crate::ty::Ty::Int };
    (float) => { $crate::ty::Ty::Float };
    (string) => { $crate::ty::Ty::String };
    (path) => { $crate::ty::Ty::Path };
    // TODO: More precise type for derivations.
    (derivation) => {
        $crate::ty::Ty::Attrset(::std::sync::Arc::new($crate::ty::Attrset::default()))
    };
    (($($inner:tt)*)) => {{ ty!($($inner)*) }};
    ([$($inner:tt)*]) => { $crate::ty::Ty::List(::std::arc::Arc::new($ty!($($inner)*)))};
    ({ $($key:literal : $ty:tt),* $(,)? }) => {{
        $crate::ty::Ty::Attrset(::std::sync::Arc::new($crate::ty::Attrset::from_internal([
            $(($key, ty!($ty)),)*
        ])))
    }};
    ($arg:tt -> $($ret:tt)*) => {
        $crate::ty::Ty::Lambda(
            ::std::sync::Arc::new(ty!($arg)),
            ::std::sync::Arc::new(ty!($($ret)*)),
        )
    };
}

mod fmt;
mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use crate::def::NameId;
use crate::{DefDatabase, FileId};
use std::sync::Arc;

pub use fmt::TyDisplay;
pub use infer::InferenceResult;
use smol_str::SmolStr;

#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase {
    #[salsa::invoke(infer::infer_query)]
    fn infer(&self, file: FileId) -> Arc<InferenceResult>;
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
    Unknown,

    // We won't wanna infer to `null` before supporting union types.
    // It would contain no information.
    // Null,
    Bool,
    Int,
    Float,
    String,
    Path,

    List(Arc<Ty>),
    Lambda(Arc<Ty>, Arc<Ty>),
    Attrset(Arc<Attrset>),
}

impl Ty {
    pub fn as_attrset(&self) -> Option<&Attrset> {
        match self {
            Self::Attrset(v) => Some(v),
            _ => None,
        }
    }

    pub fn display(&self) -> TyDisplay<'_> {
        TyDisplay::new(self, 2)
    }

    pub fn debug(&self) -> TyDisplay<'_> {
        TyDisplay::new(self, usize::MAX)
    }
}

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&TyDisplay::new(self, usize::MAX), f)
    }
}

// Invariant: sorted by names.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Attrset(Box<[(SmolStr, Ty, AttrSource)]>);

impl Attrset {
    /// Build an Attrset for internal type schemas.
    ///
    /// # Panics
    /// The given iterator must have no duplicated fields, or it'll panic.
    #[track_caller]
    // FIXME: Currently this is only used in tests.
    #[cfg_attr(not(test), allow(dead_code))]
    fn from_internal(iter: impl IntoIterator<Item = (&'static str, Ty)>) -> Self {
        let mut set = iter
            .into_iter()
            .map(|(name, ty)| (SmolStr::from(name), ty, AttrSource::Unknown))
            .collect::<Box<[_]>>();
        set.sort_by(|(lhs, ..), (rhs, ..)| lhs.cmp(rhs));
        assert!(
            set.windows(2).all(|w| w[0].0 != w[1].0),
            "Duplicated fields",
        );
        Self(set)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn get_all(&self, field: &str) -> Option<(&Ty, AttrSource)> {
        let i = self.0.binary_search_by(|p| (*p.0).cmp(field)).ok()?;
        Some((&self.0[i].1, self.0[i].2))
    }

    pub fn get(&self, field: &str) -> Option<&Ty> {
        Some(self.get_all(field)?.0)
    }

    pub fn get_src(&self, field: &str) -> Option<AttrSource> {
        Some(self.get_all(field)?.1)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SmolStr, &Ty, AttrSource)> + '_ {
        self.0.iter().map(|(k, ty, src)| (k, ty, *src))
    }
}

/// The source of an Attr.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttrSource {
    /// Unknown source, possibly generated or referenced.
    Unknown,
    /// Defined by a name.
    Name(NameId),
    // TODO: Builtins.
}
