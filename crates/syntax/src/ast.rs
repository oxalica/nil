use crate::SyntaxKind::{self, *};
use crate::{NixLanguage, SyntaxElementChildren, SyntaxNode, SyntaxToken};
use rowan::ast::support::{child, children, token};
use rowan::NodeOrToken;

pub use rowan::ast::{AstChildren, AstNode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOpKind {
    Imply,
    Or,
    And,
    Pipe,

    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    Update,
    Concat,

    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOpKind {
    Not,
    Negate,
}

pub trait HasBindings: AstNode<Language = NixLanguage> {
    fn bindings(&self) -> AstChildren<Binding> {
        children(self.syntax())
    }
}

pub trait HasStringParts: AstNode<Language = NixLanguage> {
    fn string_parts(&self) -> StringPartIter {
        StringPartIter(self.syntax().children_with_tokens())
    }
}

#[derive(Clone, Debug)]
pub struct StringPartIter(SyntaxElementChildren);

impl Iterator for StringPartIter {
    type Item = StringPart;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.find_map(|nt| match (nt.kind(), nt) {
            (STRING_FRAGMENT, NodeOrToken::Token(t)) => Some(StringPart::Fragment(t)),
            (STRING_ESCAPE, NodeOrToken::Token(t)) => Some(StringPart::Escape(t)),
            (DYNAMIC, NodeOrToken::Node(n)) => Some(StringPart::Dynamic(Dynamic(n))),
            _ => None,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringPart {
    Fragment(SyntaxToken),
    Escape(SyntaxToken),
    Dynamic(Dynamic),
}

#[derive(Clone, Debug)]
pub struct PathPartIter(SyntaxElementChildren);

impl Iterator for PathPartIter {
    type Item = PathPart;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.find_map(|nt| match (nt.kind(), nt) {
            (PATH_FRAGMENT, NodeOrToken::Token(t)) => Some(PathPart::Fragment(t)),
            (DYNAMIC, NodeOrToken::Node(n)) => Some(PathPart::Dynamic(Dynamic(n))),
            _ => None,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathPart {
    Fragment(SyntaxToken),
    Dynamic(Dynamic),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LiteralKind {
    Int,
    Float,
    Uri,
    Path,
    SearchPath,
}

trait NodeWrapper {
    const KIND: SyntaxKind;
}

macro_rules! enums {
    ($($name:ident { $($variant:ident,)* },)*) => {
        $(
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant($variant),)*
        }

        impl AstNode for $name {
            type Language = NixLanguage;

            fn can_cast(kind: SyntaxKind) -> bool
                where Self: Sized
            {
                matches!(kind, $(<$variant as NodeWrapper>::KIND)|*)
            }

            fn cast(node: SyntaxNode) -> Option<Self>
            where
                Self: Sized
            {
                match node.kind() {
                    $(<$variant as NodeWrapper>::KIND => Some(Self::$variant($variant(node))),)*
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$variant(e) => &e.0,)*
                }
            }
        }
        )*
    };
}

macro_rules! asts {
    (
        $(
            $kind:ident = $name:ident $([$trait:tt])?
            { $($impl:tt)* },
        )*
    ) => {
        $(
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub struct $name(SyntaxNode);

        impl $name {
            ast_impl!($($impl)*);
        }

        $(impl $trait for $name {})*

        impl NodeWrapper for $name {
            const KIND: SyntaxKind = SyntaxKind::$kind;
        }

        impl AstNode for $name {
            type Language = NixLanguage;

            fn can_cast(kind: SyntaxKind) -> bool
                where Self: Sized
            {
                kind == SyntaxKind::$kind
            }

            fn cast(node: SyntaxNode) -> Option<Self>
            where
                Self: Sized
            {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
        )*
    };
}

macro_rules! ast_impl {
    () => {};
    ($field:ident: $ast:ident, $($tt:tt)*) => {
        pub fn $field(&self) -> Option<$ast> { child(&self.0) }
        ast_impl!($($tt)*);
    };
    ($field:ident[$k:tt]: $ast:ident, $($tt:tt)*) => {
        pub fn $field(&self) -> Option<$ast> { children(&self.0).nth($k) }
        ast_impl!($($tt)*);
    };
    ($field:ident: [$ast:ident], $($tt:tt)*) => {
        pub fn $field(&self) -> AstChildren<$ast> { children(&self.0) }
        ast_impl!($($tt)*);
    };
    ($field:ident: T![$tok:tt], $($tt:tt)*) => {
        pub fn $field(&self) -> Option<SyntaxToken> {
            token(&self.0, T![$tok])
        }
        ast_impl!($($tt)*);
    };
    ($field:ident[$k:tt]: T![$tok:tt], $($tt:tt)*) => {
        pub fn $field(&self) -> Option<SyntaxToken> {
            self.0
                .children_with_tokens()
                .filter_map(|it| it.into_token())
                .filter(|it| it.kind() == T![$tok])
                .nth($k)
        }
        ast_impl!($($tt)*);
    };
    ($($item:item)*) => {
        $($item)*
    };
}

enums! {
    Expr {
        Apply,
        Assert,
        AttrSet,
        BinaryOp,
        HasAttr,
        IfThenElse,
        IndentString,
        Lambda,
        LetIn,
        List,
        Literal,
        Paren,
        PathInterpolation,
        Ref,
        Select,
        String,
        UnaryOp,
        With,
    },
    Attr {
        Dynamic,
        Name,
        String,
    },
    Binding {
        Inherit,
        AttrpathValue,
    },
}

impl Expr {
    pub fn flatten_paren(self) -> Option<Self> {
        let mut cur = Some(self);
        while let Some(Self::Paren(p)) = cur {
            cur = p.expr();
        }
        cur
    }

    pub fn contains_without_paren(&self, inner: &Self) -> bool {
        fn bp(e: &Expr) -> Option<u8> {
            Some(match e {
                Expr::With(_)
                | Expr::Lambda(_)
                | Expr::LetIn(_)
                | Expr::IfThenElse(_)
                | Expr::Assert(_) => TOPLEVEL,

                // Binary and unary ops. They follow `infix_bp` in parser.
                Expr::BinaryOp(e) => match e.op_kind()? {
                    BinaryOpKind::Imply => 1,
                    BinaryOpKind::Or => 3,
                    BinaryOpKind::And => 5,
                    BinaryOpKind::Pipe => 6,
                    BinaryOpKind::Equal | BinaryOpKind::NotEqual => 7,
                    BinaryOpKind::Less
                    | BinaryOpKind::Greater
                    | BinaryOpKind::LessEqual
                    | BinaryOpKind::GreaterEqual => 9,
                    BinaryOpKind::Update => 11,
                    BinaryOpKind::Add | BinaryOpKind::Sub => 15,
                    BinaryOpKind::Mul | BinaryOpKind::Div => 17,
                    BinaryOpKind::Concat => 19,
                },
                Expr::UnaryOp(e) => match e.op_kind()? {
                    UnaryOpKind::Not => 13,
                    UnaryOpKind::Negate => 23,
                },
                Expr::HasAttr(_) => 21,
                Expr::Apply(_) => 25,

                // Lists can contain Select.
                Expr::List(_) => 27,

                Expr::Select(_) => 29,

                // Atoms.
                Expr::AttrSet(_)
                | Expr::String(_)
                | Expr::IndentString(_)
                | Expr::Literal(_)
                | Expr::PathInterpolation(_)
                | Expr::Ref(_) => 29,

                // Special. See below.
                Expr::Paren(_) => PAREN,
            })
        }

        const TOPLEVEL: u8 = 0;
        const PAREN: u8 = 31;

        match (bp(self), bp(inner)) {
            // Special case 1: `Paren`s can safely contain or be contained by anything.
            (Some(PAREN), _) | (_, Some(PAREN)) => true,
            // Special case 2: top-levels can contain each other without ambiguity.
            (Some(TOPLEVEL), Some(TOPLEVEL)) => true,
            // Otherwise, expressions with lower binding power contain higher ones.
            (Some(outer), Some(inner)) => outer < inner,
            // `false` by default.
            _ => false,
        }
    }
}

asts! {
    SOURCE_FILE = SourceFile {
        expr: Expr,
    },

    APPLY = Apply {
        function: Expr,
        argument[1]: Expr,
    },
    ASSERT = Assert {
        assert_token: T![assert],
        condition: Expr,
        semicolon_token: T![;],
        body[1]: Expr,
    },
    ATTR_PATH = Attrpath {
        attrs: [Attr],
    },
    ATTR_PATH_VALUE = AttrpathValue {
        attrpath: Attrpath,
        equal_token: T![=],
        value: Expr,
        semicolon_token: T![;],
    },
    ATTR_SET = AttrSet [HasBindings] {
        l_curly_token: T!['{'],
        r_curly_token: T!['}'],

        // These two are exclusive and can only be the first non-white token.
        // Don't scan all children.
        pub fn let_token(&self) -> Option<SyntaxToken> {
            self.0
                .children_with_tokens()
                .filter_map(|it| it.into_token())
                .find(|it| !it.kind().is_trivia())
                .filter(|tok| tok.kind() == T![let])
        }
        pub fn rec_token(&self) -> Option<SyntaxToken> {
            self.0
                .children_with_tokens()
                .filter_map(|it| it.into_token())
                .find(|it| !it.kind().is_trivia())
                .filter(|tok| tok.kind() == T![rec])
        }
    },
    BINARY_OP = BinaryOp {
        lhs: Expr,
        rhs[1]: Expr,

        pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOpKind)> {
            self.syntax().children_with_tokens().find_map(|n| {
                let tok = n.into_token()?;
                let op = match tok.kind() {
                    T![->] => BinaryOpKind::Imply,
                    T![&&] => BinaryOpKind::And,
                    T![|>] => BinaryOpKind::Pipe,
                    T![||] => BinaryOpKind::Or,
                    T![==] => BinaryOpKind::Equal,
                    T![!=] => BinaryOpKind::NotEqual,
                    T![<] => BinaryOpKind::Less,
                    T![>] => BinaryOpKind::Greater,
                    T![<=] => BinaryOpKind::LessEqual,
                    T![>=] => BinaryOpKind::GreaterEqual,
                    T!["//"] => BinaryOpKind::Update,
                    T![++] => BinaryOpKind::Concat,
                    T![+] => BinaryOpKind::Add,
                    T![-] => BinaryOpKind::Sub,
                    T![*] => BinaryOpKind::Mul,
                    T![/] => BinaryOpKind::Div,
                    _ => return None,
                };
                Some((tok, op))
            })
        }
        pub fn op_token(&self) -> Option<SyntaxToken> {
            self.op_details().map(|t| t.0)
        }
        pub fn op_kind(&self) -> Option<BinaryOpKind> {
            self.op_details().map(|t| t.1)
        }
    },
    DYNAMIC = Dynamic {
        dollar_l_curly_token: T!["${"],
        expr: Expr,
        r_curly_token: T!['}'],
    },
    HAS_ATTR = HasAttr {
        set: Expr,
        question_token: T![?],
        attrpath: Attrpath,
    },
    IF_THEN_ELSE = IfThenElse {
        if_token: T![if],
        condition: Expr,
        then_token: T![then],
        then_body[1]: Expr,
        else_token: T![else],
        else_body[2]: Expr,
    },
    INDENT_STRING = IndentString [HasStringParts] {
        start_quote2_token: T!["''"],
        end_quote2_token[1]: T!["''"],
    },
    INHERIT = Inherit {
        inherit_token: T![inherit],
        from_expr: Paren,
        attrs: [Attr],
        semicolon_token: T![;],
    },
    LAMBDA = Lambda {
        param: Param,
        colon_token: T![:],
        body: Expr,
    },
    LET_IN = LetIn [HasBindings] {
        let_token: T![let],
        in_token: T![in],
        body: Expr,
    },
    LIST = List {
        l_brack_token: T!['['],
        elements: [Expr],
        r_brack_token: T![']'],
    },
    LITERAL = Literal {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }

        pub fn kind(&self) -> Option<LiteralKind> {
            Some(match self.token()?.kind() {
                INT => LiteralKind::Int,
                FLOAT => LiteralKind::Float,
                URI => LiteralKind::Uri,
                PATH => LiteralKind::Path,
                SEARCH_PATH => LiteralKind::SearchPath,
                _ => return None,
            })
        }
    },
    NAME = Name {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    PARAM = Param {
        name: Name,
        pat: Pat,
        at_token: T![@],
    },
    PAREN = Paren {
        l_brack_token: T!['('],
        expr: Expr,
        r_brack_token: T![')'],
    },
    PATH_INTERPOLATION = PathInterpolation {
        pub fn path_parts(&self) -> PathPartIter {
            PathPartIter(self.syntax().children_with_tokens())
        }
    },
    PAT = Pat {
        l_curly_token: T!['{'],
        fields: [PatField],
        ellipsis_token: T![...],
        r_curly_token: T!['}'],
    },
    PAT_FIELD = PatField {
        name: Name,
        question_token: T![?],
        default_expr: Expr,
    },
    REF = Ref {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    SELECT = Select {
        set: Expr,
        dot_token: T![.],
        attrpath: Attrpath,
        or_token: T![or],
        default_expr[1]: Expr,
    },
    STRING = String [HasStringParts] {
        start_dquote_token: T!['"'],
        end_dquote_token[1]: T!['"'],
    },
    UNARY_OP = UnaryOp {
        arg: Expr,

        pub fn op_details(&self) -> Option<(SyntaxToken, UnaryOpKind)> {
            self.syntax().children_with_tokens().find_map(|n| {
                let tok = n.into_token()?;
                let kind = match tok.kind() {
                    T![!] => UnaryOpKind::Not,
                    T![-] => UnaryOpKind::Negate,
                    _ => return None,
                };
                Some((tok, kind))
            })
        }
        pub fn op_token(&self) -> Option<SyntaxToken> {
            self.op_details().map(|t| t.0)
        }
        pub fn op_kind(&self) -> Option<UnaryOpKind> {
            self.op_details().map(|t| t.1)
        }
    },
    WITH = With {
        with_token: T![with],
        environment: Expr,
        semicolon_token: T![;],
        body[1]: Expr,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::parse;

    trait AstTest {
        fn should_eq(&self, expect: &str);
    }

    impl AstTest for SyntaxNode {
        #[track_caller]
        fn should_eq(&self, expect: &str) {
            assert_eq!(self.to_string().trim(), expect);
        }
    }

    impl AstTest for SyntaxToken {
        #[track_caller]
        fn should_eq(&self, expect: &str) {
            assert_eq!(self.to_string(), expect);
        }
    }

    #[test]
    fn apply() {
        let e = parse::<Apply>("1 2");
        e.function().unwrap().syntax().should_eq("1");
        e.argument().unwrap().syntax().should_eq("2");
    }

    #[test]
    fn assert() {
        let e = parse::<Assert>("assert 1; 2");
        e.assert_token().unwrap().should_eq("assert");
        e.condition().unwrap().syntax().should_eq("1");
        e.semicolon_token().unwrap().should_eq(";");
        e.body().unwrap().syntax().should_eq("2");
    }

    #[test]
    fn attr_path() {
        let e = parse::<Attrpath>(r#"{ foo."bar".${baz} = 1; }"#);
        let mut iter = e.attrs();
        iter.next().unwrap().syntax().should_eq("foo");
        iter.next().unwrap().syntax().should_eq(r#""bar""#);
        iter.next().unwrap().syntax().should_eq("${baz}");
        assert!(iter.next().is_none());
    }

    #[test]
    fn attr_path_value() {
        let e = parse::<AttrpathValue>(r#"{ foo."bar".${baz} = 1; }"#);
        e.attrpath()
            .unwrap()
            .syntax()
            .should_eq(r#"foo."bar".${baz}"#);
        e.equal_token().unwrap().should_eq("=");
        e.value().unwrap().syntax().should_eq("1");
        e.semicolon_token().unwrap().should_eq(";");
    }

    #[test]
    fn plain_attrset() {
        let e = parse::<AttrSet>("{ a = let { }; b = rec { }; }");
        assert!(e.let_token().is_none());
        assert!(e.rec_token().is_none());
        e.l_curly_token().unwrap().should_eq("{");
        e.r_curly_token().unwrap().should_eq("}");

        let mut iter = e.bindings();
        iter.next().unwrap().syntax().should_eq("a = let { };");
        iter.next().unwrap().syntax().should_eq("b = rec { };");
    }

    #[test]
    fn rec_attrset() {
        let e = parse::<AttrSet>("rec { a = let { }; b = rec { }; }");
        assert!(e.let_token().is_none());
        assert!(e.rec_token().is_some());
        e.l_curly_token().unwrap().should_eq("{");
        e.r_curly_token().unwrap().should_eq("}");
    }

    #[test]
    fn let_attrset() {
        let e = parse::<AttrSet>("let { a = let { }; b = rec { }; }");
        assert!(e.let_token().is_some());
        assert!(e.rec_token().is_none());
        e.l_curly_token().unwrap().should_eq("{");
        e.r_curly_token().unwrap().should_eq("}");
    }

    #[test]
    fn binary_op() {
        let e = parse::<BinaryOp>("1 + 2");
        assert_eq!(e.op_kind(), Some(BinaryOpKind::Add));
        e.op_token().unwrap().should_eq("+");
        e.lhs().unwrap().syntax().should_eq("1");
        e.rhs().unwrap().syntax().should_eq("2");
    }

    #[test]
    fn dynamic() {
        let e = parse::<Dynamic>(r#""${a}""#);
        e.dollar_l_curly_token().unwrap().should_eq("${");
        e.expr().unwrap().syntax().should_eq("a");
        e.r_curly_token().unwrap().should_eq("}");
    }

    #[test]
    fn has_attr() {
        let e = parse::<HasAttr>("a ? b.c");
        e.set().unwrap().syntax().should_eq("a");
        e.question_token().unwrap().should_eq("?");
        e.attrpath().unwrap().syntax().should_eq("b.c");
    }

    #[test]
    fn if_then_else() {
        let e = parse::<IfThenElse>("if 1 then 2 else 3");
        e.if_token().unwrap().should_eq("if");
        e.condition().unwrap().syntax().should_eq("1");
        e.then_token().unwrap().should_eq("then");
        e.then_body().unwrap().syntax().should_eq("2");
        e.else_token().unwrap().should_eq("else");
        e.else_body().unwrap().syntax().should_eq("3");
    }

    #[test]
    fn indent_string() {
        let e = parse::<IndentString>("''a''$${1}''");
        let start = e.start_quote2_token().unwrap();
        let end = e.end_quote2_token().unwrap();
        start.should_eq("''");
        end.should_eq("''");
        assert_eq!(u32::from(start.text_range().start()), 0);
        assert_eq!(u32::from(end.text_range().start()), 10);

        let mut iter = e.string_parts();
        assert!(matches!(iter.next().unwrap(), StringPart::Fragment(_)));
        assert!(matches!(iter.next().unwrap(), StringPart::Escape(_)));
        assert!(matches!(iter.next().unwrap(), StringPart::Dynamic(_)));
        assert!(iter.next().is_none());
    }

    #[test]
    fn inherit_plain() {
        let e = parse::<Inherit>("{ inherit a b; }");
        e.inherit_token().unwrap().should_eq("inherit");
        assert!(e.from_expr().is_none());
        e.semicolon_token().unwrap().should_eq(";");

        let mut iter = e.attrs();
        iter.next().unwrap().syntax().should_eq("a");
        iter.next().unwrap().syntax().should_eq("b");
        assert!(iter.next().is_none());
    }

    #[test]
    fn inherit_from() {
        let e = parse::<Inherit>("{ inherit (x) a b; }");
        e.inherit_token().unwrap().should_eq("inherit");
        e.from_expr().unwrap().syntax().should_eq("(x)");
        e.semicolon_token().unwrap().should_eq(";");

        let mut iter = e.attrs();
        iter.next().unwrap().syntax().should_eq("a");
        iter.next().unwrap().syntax().should_eq("b");
        assert!(iter.next().is_none());
    }

    #[test]
    fn lambda() {
        let e = parse::<Lambda>("a: b");
        e.param().unwrap().syntax().should_eq("a");
        e.colon_token().unwrap().should_eq(":");
        e.body().unwrap().syntax().should_eq("b");
    }

    #[test]
    fn let_in() {
        let e = parse::<LetIn>("let a = 1; in b");
        e.let_token().unwrap().should_eq("let");
        e.in_token().unwrap().should_eq("in");

        let mut iter = e.bindings();
        iter.next().unwrap().syntax().should_eq("a = 1;");
        assert!(iter.next().is_none());
    }

    #[test]
    fn list() {
        let e = parse::<List>("[ a b ]");
        e.l_brack_token().unwrap().should_eq("[");
        e.r_brack_token().unwrap().should_eq("]");

        let mut iter = e.elements();
        iter.next().unwrap().syntax().should_eq("a");
        iter.next().unwrap().syntax().should_eq("b");
        assert!(iter.next().is_none());
    }

    #[test]
    fn literal() {
        for (src, k) in [
            ("1", LiteralKind::Int),
            ("1.e2", LiteralKind::Float),
            ("a:b", LiteralKind::Uri),
            ("/.", LiteralKind::Path),
            ("<a>", LiteralKind::SearchPath),
        ] {
            let e = parse::<Literal>(src);
            e.token().unwrap().should_eq(src);
            assert_eq!(e.kind(), Some(k));
        }
    }

    #[test]
    fn name() {
        let e = parse::<Name>("{ a = 1; }");
        e.token().unwrap().should_eq("a");
    }

    #[test]
    fn param_name() {
        let e = parse::<Param>("a: b");
        e.name().unwrap().syntax().should_eq("a");
        assert!(e.at_token().is_none());
        assert!(e.pat().is_none());
    }

    #[test]
    fn param_pat() {
        let e = parse::<Param>("{ }: b");
        assert!(e.name().is_none());
        assert!(e.at_token().is_none());
        e.pat().unwrap().syntax().should_eq("{ }");
    }

    #[test]
    fn param_name_pat() {
        let e = parse::<Param>("a @ { }: b");
        e.name().unwrap().syntax().should_eq("a");
        e.at_token().unwrap().should_eq("@");
        e.pat().unwrap().syntax().should_eq("{ }");
    }

    #[test]
    fn param_pat_name() {
        let e = parse::<Param>("{ } @ a: b");
        e.name().unwrap().syntax().should_eq("a");
        e.at_token().unwrap().should_eq("@");
        e.pat().unwrap().syntax().should_eq("{ }");
    }

    #[test]
    fn paren() {
        let e = parse::<Paren>("(1)");
        e.l_brack_token().unwrap().should_eq("(");
        e.expr().unwrap().syntax().should_eq("1");
        e.r_brack_token().unwrap().should_eq(")");
    }

    #[test]
    fn path_interpolation() {
        let e = parse::<PathInterpolation>("/${a}b/c");
        let mut iter = e.path_parts();
        assert!(matches!(iter.next(), Some(PathPart::Fragment(_))));
        assert!(matches!(iter.next(), Some(PathPart::Dynamic(_))));
        assert!(matches!(iter.next(), Some(PathPart::Fragment(_))));
        assert!(iter.next().is_none());
    }

    #[test]
    fn pat_exhaustive() {
        let e = parse::<Pat>("{ a, b }: 1");
        e.l_curly_token().unwrap().should_eq("{");
        e.r_curly_token().unwrap().should_eq("}");
        assert!(e.ellipsis_token().is_none());

        let mut iter = e.fields();
        iter.next().unwrap().syntax().should_eq("a");
        iter.next().unwrap().syntax().should_eq("b");
    }

    #[test]
    fn pat_ellipsis() {
        let e = parse::<Pat>("{ a, b, ... }: 1");
        e.ellipsis_token().unwrap().should_eq("...");

        let mut iter = e.fields();
        iter.next().unwrap().syntax().should_eq("a");
        iter.next().unwrap().syntax().should_eq("b");
    }

    #[test]
    fn pat_field_simple() {
        let e = parse::<PatField>("{ a }: 1");
        e.name().unwrap().syntax().should_eq("a");
        assert!(e.question_token().is_none());
        assert!(e.default_expr().is_none());
    }

    #[test]
    fn pat_field_default() {
        let e = parse::<PatField>("{ a ? b }: 1");
        e.name().unwrap().syntax().should_eq("a");
        e.question_token().unwrap().should_eq("?");
        e.default_expr().unwrap().syntax().should_eq("b");
    }

    #[test]
    fn ref_() {
        let e = parse::<Ref>("a");
        e.token().unwrap().should_eq("a");
    }

    #[test]
    fn select_simple() {
        let e = parse::<Select>("a.b.c");
        e.set().unwrap().syntax().should_eq("a");
        e.dot_token().unwrap().should_eq(".");
        assert_eq!(u32::from(e.dot_token().unwrap().text_range().start()), 1);
        e.attrpath().unwrap().syntax().should_eq("b.c");
        assert!(e.or_token().is_none());
        assert!(e.default_expr().is_none());
    }

    #[test]
    fn select_default() {
        let e = parse::<Select>("a.b.c or d");
        e.set().unwrap().syntax().should_eq("a");
        e.dot_token().unwrap().should_eq(".");
        assert_eq!(u32::from(e.dot_token().unwrap().text_range().start()), 1);
        e.attrpath().unwrap().syntax().should_eq("b.c");
        e.or_token().unwrap().should_eq("or");
        e.default_expr().unwrap().syntax().should_eq("d");
    }

    #[test]
    fn string() {
        let e = parse::<String>(r#""a\n${b}""#);
        let start = e.start_dquote_token().unwrap();
        let end = e.end_dquote_token().unwrap();
        start.should_eq("\"");
        end.should_eq("\"");
        assert_eq!(u32::from(start.text_range().start()), 0);
        assert_eq!(u32::from(end.text_range().start()), 8);

        let mut iter = e.string_parts();
        assert!(matches!(iter.next().unwrap(), StringPart::Fragment(_)));
        assert!(matches!(iter.next().unwrap(), StringPart::Escape(_)));
        assert!(matches!(iter.next().unwrap(), StringPart::Dynamic(_)));
        assert!(iter.next().is_none());
    }

    #[test]
    fn unary_op() {
        let e = parse::<UnaryOp>("-1");
        assert_eq!(e.op_kind(), Some(UnaryOpKind::Negate));
        e.op_token().unwrap().should_eq("-");
        e.arg().unwrap().syntax().should_eq("1");
    }

    #[test]
    fn with() {
        let e = parse::<With>("with 1; 2");
        e.with_token().unwrap().should_eq("with");
        e.environment().unwrap().syntax().should_eq("1");
        e.semicolon_token().unwrap().should_eq(";");
        e.body().unwrap().syntax().should_eq("2");
    }
}
