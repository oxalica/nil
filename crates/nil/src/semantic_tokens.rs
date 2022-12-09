use ide::{BuiltinKind, HlKeyword, HlPunct, HlTag, NameKind};
use lsp_types::{SemanticTokenModifier, SemanticTokenType};

macro_rules! def_index {
    (
        $ty:ty, $array:ident, $enum:ident;
        $($ident:ident => $expr:expr,)*
    ) => {
        pub const $array: &[$ty] = &[$($expr),*];

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum $enum { $($ident),* }
    };
}

// coc.nvim's builtin highlighting groups:
// https://github.com/neoclide/coc.nvim/blob/347e33d77bf58fedd32ef4eb1dc982a11f5f0a22/plugin/coc.vim#L527
def_index! {
    SemanticTokenType, SEMANTIC_TOKEN_TYPES, TokenTypeIdx;

    Comment => SemanticTokenType::COMMENT,
    Constant => SemanticTokenType::new("constant"),
    Function => SemanticTokenType::FUNCTION,
    Keyword => SemanticTokenType::KEYWORD,
    Number => SemanticTokenType::NUMBER,
    Operator => SemanticTokenType::OPERATOR,
    Parameter => SemanticTokenType::PARAMETER,
    Path => SemanticTokenType::new("path"),
    Property => SemanticTokenType::PROPERTY,
    Punctuation => SemanticTokenType::new("punctuation"),
    String => SemanticTokenType::STRING,
    Struct => SemanticTokenType::STRUCT,
    Variable => SemanticTokenType::VARIABLE,
}

def_index! {
    SemanticTokenModifier, SEMANTIC_TOKEN_MODIFIERS, TokenModIdx;

    Builtin => SemanticTokenModifier::new("builtin"),
    Conditional => SemanticTokenModifier::new("conditional"),
    Definition => SemanticTokenModifier::DEFINITION,
    Delimiter => SemanticTokenModifier::new("delimiter"),
    Escape => SemanticTokenModifier::new("escape"),
    Parenthesis => SemanticTokenModifier::new("parenthesis"),
    Readonly => SemanticTokenModifier::READONLY,
    Unresolved => SemanticTokenModifier::new("unresolved"),
}

impl TokenModIdx {
    pub fn to_bit(self) -> u32 {
        1 << self as u8
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenModSet(pub u32);

impl TokenModSet {
    pub fn insert(&mut self, i: TokenModIdx) {
        self.0 |= i.to_bit();
    }
}

pub(crate) fn to_semantic_type_and_modifiers(tag: HlTag) -> (TokenTypeIdx, TokenModSet) {
    let mut mods = TokenModSet::default();
    let ty = match tag {
        HlTag::NameDef(kind) | HlTag::NameRef(kind) => {
            if matches!(tag, HlTag::NameDef(_)) {
                mods.insert(TokenModIdx::Definition);
            }
            match kind {
                NameKind::LetIn => TokenTypeIdx::Variable,
                NameKind::PlainAttrset | NameKind::RecAttrset => TokenTypeIdx::Property,
                // TODO: Split out `PatUniversal`?
                NameKind::Param | NameKind::PatField => TokenTypeIdx::Parameter,
            }
        }
        HlTag::UnresolvedRef => {
            mods.insert(TokenModIdx::Unresolved);
            TokenTypeIdx::Variable
        }
        HlTag::AttrField => TokenTypeIdx::Property,
        HlTag::Builtin(kind) => {
            mods.insert(TokenModIdx::Builtin);
            match kind {
                BuiltinKind::Const => {
                    mods.insert(TokenModIdx::Readonly);
                    TokenTypeIdx::Constant
                }
                BuiltinKind::Function => TokenTypeIdx::Function,
                BuiltinKind::Attrset => TokenTypeIdx::Struct,
            }
        }
        HlTag::Comment => TokenTypeIdx::Comment,
        HlTag::StringEscape => {
            mods.insert(TokenModIdx::Escape);
            TokenTypeIdx::String
        }
        HlTag::FloatLiteral | HlTag::IntLiteral => TokenTypeIdx::Number,
        HlTag::Keyword(kw) => match kw {
            HlKeyword::Conditional => {
                mods.insert(TokenModIdx::Conditional);
                TokenTypeIdx::Keyword
            }
            HlKeyword::Operator => TokenTypeIdx::Operator,
            HlKeyword::Other => TokenTypeIdx::Keyword,
        },
        // TODO.
        HlTag::Operator(_) => TokenTypeIdx::Operator,
        HlTag::Path => TokenTypeIdx::Path,
        // TODO.
        HlTag::Punct(punct) => {
            match punct {
                HlPunct::Brace | HlPunct::Bracket | HlPunct::Paren => {
                    mods.insert(TokenModIdx::Parenthesis)
                }
                HlPunct::Dot
                | HlPunct::Question
                | HlPunct::Comma
                | HlPunct::Semicolon
                | HlPunct::Colon
                | HlPunct::Equal
                | HlPunct::At => mods.insert(TokenModIdx::Delimiter),
                HlPunct::Ellipsis => {}
            }
            TokenTypeIdx::Punctuation
        }
        HlTag::StringLiteral => TokenTypeIdx::String,
    };
    (ty, mods)
}
