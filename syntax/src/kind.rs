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

    // Symbols len=1.
    AT = [@] @SYMBOL_FIRST,
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

    // Symbols len=2.
    AND2 = [&&],
    DOLLAR_L_CURLY = ["${"],
    EQ2 = [==],
    GT_EQ = [>=],
    LT_EQ = [<=],
    MINUS_GT = [->],
    NOT_EQ = [!=],
    OR2 = [||],
    PLUS2 = [++],
    QUOTE2 = ["''"],
    SLASH2 = ["//"],

    // Symbols len=3.
    DOT3 = [...] @SYMBOL_LAST,

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
    #[inline(always)]
    pub fn is_whitespace(self) -> bool {
        matches!(self, Self::COMMENT | Self::SPACE)
    }

    #[inline(always)]
    pub fn is_keyword(self) -> bool {
        (Self::KEYWORD_FIRST as u8..=Self::KEYWORD_LAST as u8).contains(&(self as u8))
    }

    #[inline(always)]
    pub fn is_symbol(self) -> bool {
        (Self::SYMBOL_FIRST as u8..=Self::SYMBOL_LAST as u8).contains(&(self as u8))
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
