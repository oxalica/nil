use std::fmt;

macro_rules! def {
    (
        $(
            $(#[$meta:meta])*
            $variant:ident $(= [$($tt:tt)*])? $(@ $anchor:ident)?,
        )*
    ) => {
        #[allow(bad_style)]
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        #[repr(u8)]
        #[non_exhaustive]
        pub enum SyntaxKind {
            $(
                $(#[$meta])*
                $variant,
            )*
        }

        #[macro_export]
        macro_rules! T {
            $($(
                ($($tt)*) => { $crate::SyntaxKind::$variant };
            )?)*
        }

        impl SyntaxKind {
            $($(const $anchor: Self = Self::$variant;)?)*
        }

        impl fmt::Display for SyntaxKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(Self::$variant => f.write_str(to_str!($variant, $($($tt)*)?)),)*
                }
            }
        }
    };
}

macro_rules! to_str {
    // IDENT
    ($variant:tt, ) => {
        concat!('"', stringify!($variant), '"')
    };
    // Special case.
    ($variant:tt, '"') => {
        r#"'"'"#
    };
    // This breaks `literal` fragment.
    ($variant:tt, -) => {
        r#""-""#
    };
    // '['
    ($variant:tt, $s:literal) => {
        concat!('"', $s, '"')
    };
    // &&
    ($variant:tt, $($tt:tt)+) => {
        concat!('"', stringify!($($tt)+), '"')
    };
}

def! {
    // Placeholder.
    ERROR @FIRST,

    // Whitespace.
    SPACE,
    COMMENT,

    // Keywords.
    KW_ASSERT = [assert] @KEYWORD_FIRST,
    KW_ELSE = [else],
    KW_IF = [if],
    KW_IN = [in],
    KW_INHERIT = [inherit],
    KW_LET = [let],
    KW_OR = [or],
    KW_REC = [rec],
    KW_THEN = [then],
    KW_WITH = [with] @KEYWORD_LAST,

    // Punctuations len=1.
    AT = [@] @PUNCT_FIRST,
    BANG = [!],
    COLON = [:],
    COMMA = [,],
    DOT = [.],
    DQUOTE = ['"'],
    EQ = [=],
    GT = [>],
    LT = [<],
    L_BRACK = ['['],
    L_CURLY = ['{'],
    L_PAREN = ['('],
    MINUS = [-],
    PLUS = [+],
    QUESTION = [?],
    R_BRACK = [']'],
    R_CURLY = ['}'],
    R_PAREN = [')'],
    SEMICOLON = [;],
    SLASH = [/],
    STAR = [*],

    // Punctuations len=2.
    AND2 = [&&],
    DOLLAR_L_CURLY = ["${"],
    EQ2 = [==],
    GT_EQ = [>=],
    LT_EQ = [<=],
    MINUS_GT = [->],
    NOT_EQ = [!=],
    PIPE = [|>],
    OR2 = [||],
    PLUS2 = [++],
    QUOTE2 = ["''"],
    SLASH2 = ["//"],

    // Punctuations len=3.
    DOT3 = [...] @PUNCT_LAST,

    // Literals and identifiers.
    FLOAT,
    IDENT,
    INT,
    PATH,
    SEARCH_PATH,
    URI,

    // Path interpolation.
    PATH_START,
    PATH_END,
    PATH_FRAGMENT,

    // String parts.
    STRING_FRAGMENT,
    STRING_ESCAPE,

    // Entry node.
    SOURCE_FILE,

    // Other nodes.
    APPLY,
    ASSERT,
    ATTR_PATH,
    ATTR_PATH_VALUE,
    ATTR_SET,
    BINARY_OP,
    DYNAMIC,
    HAS_ATTR,
    IF_THEN_ELSE,
    INDENT_STRING,
    INHERIT,
    LAMBDA,
    LET_IN,
    LIST,
    LITERAL,
    NAME,
    PARAM,
    PAREN,
    PAT,
    PATH_INTERPOLATION,
    PAT_FIELD,
    REF,
    SELECT,
    STRING,
    UNARY_OP,
    WITH @LAST,
}

impl SyntaxKind {
    /// Returns whether this is a SPACE.
    #[inline(always)]
    pub fn is_space(self) -> bool {
        matches!(self, Self::SPACE)
    }

    /// Returns whether this is a COMMENT or SPACE.
    #[inline(always)]
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::COMMENT | Self::SPACE)
    }

    /// Returns whether this is a keyword.
    /// Contextual keywords are not considered keywords outside expected contexts.
    #[inline(always)]
    pub fn is_keyword(self) -> bool {
        (Self::KEYWORD_FIRST as u8..=Self::KEYWORD_LAST as u8).contains(&(self as u8))
    }

    /// Returns whether this is a punctuation, including operators and delimiters.
    #[inline(always)]
    pub fn is_punct(self) -> bool {
        (Self::PUNCT_FIRST as u8..=Self::PUNCT_LAST as u8).contains(&(self as u8))
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    #[inline]
    fn from(k: SyntaxKind) -> Self {
        Self(k as u16)
    }
}

impl From<rowan::SyntaxKind> for SyntaxKind {
    #[inline(always)]
    fn from(k: rowan::SyntaxKind) -> Self {
        assert!((Self::FIRST as u16..=Self::LAST as u16).contains(&k.0));
        // SAFETY: Guarded by the assert.
        unsafe { std::mem::transmute::<u8, SyntaxKind>(k.0 as u8) }
    }
}
