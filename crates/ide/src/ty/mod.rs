#[rustfmt::skip]
#[macro_export]
macro_rules! ty {
    (?) => { $crate::ty::Ty::Unknown };
    (!) => { $crate::ty::Ty::Unknown };
    (bool) => { $crate::ty::Ty::Bool };
    (int) => { $crate::ty::Ty::Int };
    (float) => { $crate::ty::Ty::Float };
    (string) => { $crate::ty::Ty::String };
    (regex) => { $crate::ty::Ty::String };
    (path) => { $crate::ty::Ty::Path };
    (# $e:expr) => { $e };

    (derivation) => { $crate::ty::known::DERIVATION.clone() };

    // TODO: Union type.
    (number) => { $crate::ty::Ty::Float };
    (stringish) => { $crate::ty::Ty::String };
    ($ty:tt | $($rest:tt)|+) => {{
        $(let _ = ty!($rest);)+
        ty!($ty)
    }};

    // TODO: Polymorphism.
    (forall a $(b)?, $($ty:tt)*) => { ty!($($ty)*) };
    (a) => { $crate::ty::Ty::Unknown };
    (b) => { $crate::ty::Ty::Unknown };

    (($($inner:tt)*)) => { ty!($($inner)*) };
    ([$($inner:tt)*]) => { $crate::ty::Ty::List(::std::sync::Arc::new(ty!($($inner)*)))};
    ({ $($key:literal : $ty:tt),* $(,)? $(_ : $rest_ty:tt)? }) => {{
        // TODO: Rest type.
        $(let _ = ty!($rest_ty);)?
        $crate::ty::Ty::Attrset($crate::ty::Attrset::from_internal([
            $(($key, ty!($ty), $crate::ty::AttrSource::Unknown),)*
        ]))
    }};
    ({($src:expr) $($key:literal : $ty:tt),* $(,)? $(_ : $rest_ty:tt)? }) => {{
        $(let _ = ty!($rest_ty);)?
        $crate::ty::Ty::Attrset($crate::ty::Attrset::from_internal([
            $(($key, ty!($ty), $src),)*
        ]))
    }};
    ($arg:tt -> $($ret:tt)*) => {
        $crate::ty::Ty::Lambda(
            ::std::sync::Arc::new(ty!($arg)),
            ::std::sync::Arc::new(ty!($($ret)*)),
        )
    };
}

mod display;
mod infer;
pub mod known;
mod union_find;

#[cfg(test)]
mod tests;

use crate::def::NameId;
use crate::{DefDatabase, FileId, ModuleKind};
use std::fmt;
use std::sync::Arc;

pub use display::{Config as DisplayConfig, TyDisplay};
pub use infer::InferenceResult;
use smol_str::SmolStr;

#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase {
    #[salsa::invoke(module_expected_ty)]
    fn module_expected_ty(&self, file: FileId) -> Option<Ty>;

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
    Attrset(Attrset),
}

impl Ty {
    pub fn is_known(&self) -> bool {
        !matches!(self, Self::Unknown)
    }

    pub fn as_attrset(&self) -> Option<&Attrset> {
        match self {
            Self::Attrset(v) => Some(v),
            _ => None,
        }
    }

    pub fn display_with(&self, config: display::Config) -> TyDisplay<'_> {
        TyDisplay::new(self, config)
    }

    pub fn debug(&self) -> TyDisplay<'_> {
        self.display_with(display::Config::FULL)
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.debug(), f)
    }
}

// Invariant: sorted by names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attrset(Arc<[(SmolStr, Ty, AttrSource)]>);

impl Default for Attrset {
    fn default() -> Self {
        Self(Arc::new([]))
    }
}

impl Attrset {
    /// Build an Attrset for internal type schemas.
    ///
    /// # Panics
    /// The given iterator must have no duplicated fields, or it'll panic.
    #[track_caller]
    pub fn from_internal<'a>(iter: impl IntoIterator<Item = (&'a str, Ty, AttrSource)>) -> Self {
        let mut set = iter
            .into_iter()
            .map(|(name, ty, src)| (SmolStr::from(name), ty, src))
            .collect::<Arc<[_]>>();
        Arc::get_mut(&mut set)
            .unwrap()
            .sort_by(|(lhs, ..), (rhs, ..)| lhs.cmp(rhs));
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
    /// A builtin name.
    Builtin,
}

fn module_expected_ty(db: &dyn TyDatabase, file: FileId) -> Option<Ty> {
    match &*db.module_kind(file) {
        ModuleKind::Unknown => None,
        ModuleKind::FlakeNix {
            explicit_inputs,
            param_inputs,
            ..
        } => {
            let mut inputs = explicit_inputs
                .keys()
                .chain(param_inputs.keys())
                .map(|s| &**s)
                .collect::<Vec<_>>();
            inputs.sort();
            inputs.dedup();
            Some(known::flake(&inputs))
        }
        ModuleKind::Package { .. } => Some(known::PACKAGE.clone()),
        ModuleKind::ConfigModule { .. } => Some(known::CONFIG_MODULE.clone()),
        ModuleKind::Config { .. } => Some(known::CONFIG.clone()),
    }
}
