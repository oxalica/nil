use crate::SyntaxKind::{self, *};
use crate::{NixLanguage, SyntaxElementChildren, SyntaxNode, SyntaxToken};
use rowan::ast::support::{child, children, token};
use rowan::ast::{AstChildren, AstNode};
use rowan::NodeOrToken;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOpKind {
    Imply,
    Or,
    And,

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
    },
    ATTR_SET = AttrSet [HasBindings] {
        rec_token: T![rec],
        let_token: T![let],
        l_curly_token: T!['{'],
        r_curly_token: T!['}'],
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
        fields: [PatField],
        ellipsis_token: T![...],
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
