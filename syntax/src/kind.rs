macro_rules! def {
    (
        $(
            $(#[$meta:meta])*
            $variant:ident $(= [$($tt:tt)*])?,
        )*
    ) => {
        #[allow(bad_style)]
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        #[repr(u16)]
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
    };
}

def! {
    // Placeholder.
    ERROR,

    // Entry node.
    SOURCE_FILE,

    // Whitespace.
    SPACE,
    COMMENT,

    // Keywords.
    KW_ASSERT = [assert],
    KW_ELSE = [else],
    KW_IF = [if],
    KW_IN = [in],
    KW_INHERIT = [inherit],
    KW_LET = [let],
    KW_OR = [or],
    KW_REC = [rec],
    KW_THEN = [then],
    KW_WITH = [with],

    // Symbols len=1.
    AT = [@],
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
    DOT3 = [...],

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

    // Nodes.
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
    WITH,

    #[doc(hidden)]
    __LAST,
}

impl From<u16> for SyntaxKind {
    #[inline]
    fn from(d: u16) -> SyntaxKind {
        assert!(d <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
    }
}

impl From<SyntaxKind> for u16 {
    #[inline]
    fn from(k: SyntaxKind) -> u16 {
        k as u16
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    #[inline]
    fn from(k: SyntaxKind) -> Self {
        Self(k as u16)
    }
}

impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(k: rowan::SyntaxKind) -> Self {
        assert!(k.0 <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(k.0) }
    }
}
