use crate::SyntaxKind::{self, *};
use once_cell::sync::Lazy;
use regex_automata::dfa::{dense, Automaton, StartKind};
use regex_automata::nfa::thompson::Config as NfaConfig;
use regex_automata::util::primitives::StateID;
use regex_automata::util::syntax::Config as SyntaxConfig;
use regex_automata::Anchored;
use rowan::{TextRange, TextSize};
use std::ptr;

pub const KEYWORDS: &[(&str, SyntaxKind)] = &[
    ("assert", T![assert]),
    ("else", T![else]),
    ("if", T![if]),
    ("in", T![in]),
    ("inherit", T![inherit]),
    ("let", T![let]),
    ("or", T![or]),
    ("rec", T![rec]),
    ("then", T![then]),
    ("with", T![with]),
];

struct Dfa {
    dfa: dense::DFA<Vec<u32>>,
    pat_map: &'static [SyntaxKind],
    start: StateID,
}

impl Dfa {
    fn new(pats: &[&str], pat_map: &'static [SyntaxKind]) -> Self {
        let dfa = dense::Builder::new()
            .configure(
                dense::Config::new()
                    .minimize(true)
                    .start_kind(StartKind::Anchored),
            )
            .syntax(SyntaxConfig::new().unicode(false).utf8(false))
            .thompson(NfaConfig::new().utf8(false).shrink(true))
            .build_many(pats)
            .unwrap();
        let start = dfa
            .universal_start_state(Anchored::Yes)
            .expect("no look-around");
        Self {
            dfa,
            pat_map,
            start,
        }
    }

    // `Dfa::try_search_fwd` is >50% slower than manually executing DFA on regex-automata 0.3.
    // See: https://github.com/rust-lang/regex/issues/1029#issuecomment-1626453062
    fn execute(&self, haystack: &[u8]) -> Option<(StateID, usize)> {
        let mut sid = self.start;
        let mut ret = None;
        for (i, byte) in haystack.iter().copied().enumerate() {
            sid = self.dfa.next_state(sid, byte);
            if self.dfa.is_match_state(sid) {
                ret = Some((sid, i));
            } else if self.dfa.is_dead_state(sid) {
                return ret;
            }
        }
        sid = self.dfa.next_eoi_state(sid);
        if self.dfa.is_match_state(sid) {
            return Some((sid, haystack.len()));
        }
        ret
    }

    fn match_first(&self, haystack: &[u8]) -> Option<(SyntaxKind, usize)> {
        let (sid, pos) = self.execute(haystack)?;
        let pid = self.dfa.match_pattern(sid, 0);
        let kind = self.pat_map[pid.as_usize()];
        Some((kind, pos))
    }
}

macro_rules! regex_dfa {
    ($static:ident { $($tok:path = $regex:literal,)* }) => {
        static $static: Lazy<Dfa> = Lazy::new(|| Dfa::new(
            &[$($regex),*],
            &[$($tok),*],
        ));
    };
}

regex_dfa! {
    DEFAULT_TOKEN_DFA {
        // The order matters!
        SPACE = r"[ \r\n\t]+",
        COMMENT = r"#.*|/\*([^*]|\*+[^/*])*\*+/",
        // N.B. Nix somehow accepts multiple slashes in path interpolation except the first
        // slash, but the first path fragment accepts at most 2 continuous slashes.
        //
        // Here we try to accept multiple and trailing slashes except the first one,
        // and phrase them out in the parser to avoid parser abuse :)
        PATH_START = r"(~|[a-zA-Z0-9._+-]*)/([a-zA-Z0-9._+-][/a-zA-Z0-9._+-]*)?\$\{",
        PATH = r"(~|[a-zA-Z0-9._+-]*)/[a-zA-Z0-9._+-][/a-zA-Z0-9._+-]*",
        SEARCH_PATH = r"<[a-zA-Z0-9._+-]+(/[a-zA-Z0-9._+-]+)*>",
        FLOAT = r"(\d+\.\d*|\.\d+)([Ee][+-]?\d+)?",
        INT = r"\d+",
        URI = r"[a-zA-Z][a-zA-Z0-9.+-]*:[a-zA-Z0-9%/?:@&=+$,_.!~*'-]+",
        // This should match `crate::semantic::is_valid_ident`.
        IDENT = r"[a-zA-Z_][a-zA-Z0-9_'-]*",

        DQUOTE = "\"",
        QUOTE2 = r"''",

        DOT3 = r"\.\.\.",
        MINUS_GT = r"->",
        PIPE = r"\|>",
        OR2 = r"\|\|",
        AND2 = r"&&",
        EQ2 = r"==",
        NOT_EQ = r"!=",
        LT_EQ = r"<=",
        GT_EQ = r">=",
        SLASH2 = r"//",
        PLUS2 = r"\+\+",
        DOLLAR_L_CURLY = r"\$\{",
        L_CURLY = r"\{",
        R_CURLY = r"}",
        L_BRACK = r"\[",
        R_BRACK = r"]",
        L_PAREN = r"\(",
        R_PAREN = r"\)",
        AT = r"@",
        COLON = r":",
        SEMICOLON = r";",
        COMMA = r",",
        QUESTION = r"\?",
        PLUS = r"\+",
        MINUS = r"-",
        STAR = r"\*",
        DOT = r"\.",
        SLASH = r"/",
        LT = r"<",
        GT = r">",
        BANG = r"!",
        EQ = r"=",
    }
}

regex_dfa! {
    STRING_TOKEN_DFA {
        // The order matters!
        DQUOTE = r#"""#,
        // Yes, we parse one UTF-8 encoded char here, to avoid break into code units.
        // We can assume the input is already a valid UTF-8 string.
        STRING_ESCAPE = r"\\([\x00-\x7F]|[\x80-\xFF][\x80-\xBF]*)",
        DOLLAR_L_CURLY = r"\$\{",
        // `$$` makes the second `$` loses the special meaning.
        STRING_FRAGMENT = r"\$\$",
        // Otherwise, treat it literally.
        STRING_FRAGMENT = r"(?s).",
    }
}

regex_dfa! {
    INDENT_STRING_TOKEN_DFA {
        // The order matters!
        // See comments in STRING_TOKEN_DFA's STRING_ESCAPE.
        STRING_ESCAPE = r"''\\([\x00-\x7F]|[\x80-\xFF][\x80-\xBF]*)|''\$|'''",
        QUOTE2 = r"''",
        DOLLAR_L_CURLY = r"\$\{",
        // `$$` makes the second `$` loses the special meaning.
        STRING_FRAGMENT = r"\$\$",
        // Otherwise, treat it literally.
        STRING_FRAGMENT = r"(?s).",
    }
}

regex_dfa! {
    PATH_TOKEN_DFA {
        DOLLAR_L_CURLY = r"\$\{",
        PATH_FRAGMENT = r"[/a-zA-Z0-9._+-]+",
    }
}

pub type LexTokens = Vec<(SyntaxKind, TextRange)>;

/// Tokenize the source of a Nix file.
///
/// # Panics
/// Panic if the source is longer than `u32::MAX`.
pub fn lex(src: &[u8]) -> LexTokens {
    assert!(u32::try_from(src.len()).is_ok());

    let total_len = TextSize::try_from(src.len()).expect("Length overflow");

    let default_ctx = &*DEFAULT_TOKEN_DFA;
    let string_ctx = &*STRING_TOKEN_DFA;
    let indent_string_ctx = &*INDENT_STRING_TOKEN_DFA;
    let path_ctx = &*PATH_TOKEN_DFA;

    let mut out = Vec::new();
    let mut ctxs = Vec::new();

    let mut offset = TextSize::from(0);
    while offset != total_len {
        let dfa = ctxs.last().copied().unwrap_or(default_ctx);

        let rest = &src[usize::from(offset)..];
        let (mut tok, mut len) = match dfa.match_first(rest) {
            // Offset <= u32, already checked.
            Some((kind, len)) => (kind, TextSize::from(len as u32)),
            None if ptr::eq(dfa, path_ctx) => {
                ctxs.pop().expect("In path");
                out.push((PATH_END, TextRange::empty(offset)));
                continue;
            }
            None => {
                let char_len = match rest[0] {
                    0b0000_0000..=0b0111_1111 => TextSize::from(1),
                    0b1100_0000..=0b1101_1111 => TextSize::from(2),
                    0b1110_0000..=0b1110_1111 => TextSize::from(3),
                    _ => TextSize::from(4),
                };
                out.push((ERROR, TextRange::at(offset, char_len)));
                offset += char_len;
                continue;
            }
        };

        match tok {
            T!['"'] | T!["''"] if !ptr::eq(dfa, default_ctx) => {
                ctxs.pop();
            }
            T!['"'] => ctxs.push(string_ctx),
            T!["''"] => ctxs.push(indent_string_ctx),
            T!['{'] | T!["${"] => ctxs.push(default_ctx),
            T!['}'] => {
                ctxs.pop();
            }
            IDENT => {
                let ident = &rest[..usize::from(len)];
                tok = KEYWORDS
                    .iter()
                    .find_map(|&(kw, tok)| (kw.as_bytes() == ident).then_some(tok))
                    .unwrap_or(IDENT);
            }
            PATH_START => {
                len -= TextSize::of("${");
                ctxs.push(path_ctx);
                out.push((PATH_START, TextRange::empty(offset)));
                tok = PATH_FRAGMENT;
            }
            STRING_FRAGMENT => {
                // Merge continuous fragments.
                if let Some((STRING_FRAGMENT, range)) = out.last_mut() {
                    offset += len;
                    *range = TextRange::new(range.start(), offset);
                    continue;
                }
            }
            _ => {}
        }

        out.push((tok, TextRange::at(offset, len)));
        offset += len;
    }

    if matches!(ctxs.last(), Some(&dfa) if ptr::eq(dfa, path_ctx)) {
        out.push((PATH_END, TextRange::empty(total_len)));
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_lex(src: &str, expect: Expect) {
        let toks = lex(src.as_bytes());
        let out = toks
            .iter()
            .map(|(tok, range)| format!("{:?} {:?}\n", tok, &src[*range]))
            .collect::<Vec<_>>()
            .join("");
        expect.assert_eq(&out);
    }

    #[test]
    fn basic() {
        check_lex(
            "123 abc-def -> let - in' /*\n *orz\n */ #foo\n 4.5e-3",
            expect![[r##"
                INT "123"
                SPACE " "
                IDENT "abc-def"
                SPACE " "
                MINUS_GT "->"
                SPACE " "
                KW_LET "let"
                SPACE " "
                MINUS "-"
                SPACE " "
                IDENT "in'"
                SPACE " "
                COMMENT "/*\n *orz\n */"
                SPACE " "
                COMMENT "#foo"
                SPACE "\n "
                FLOAT "4.5e-3"
            "##]],
        );
    }

    #[test]
    fn uri() {
        check_lex(
            "a: b:c https://user@example.com:8080/a-b?c=d&e=%ff#comment",
            expect![[r##"
                IDENT "a"
                COLON ":"
                SPACE " "
                URI "b:c"
                SPACE " "
                URI "https://user@example.com:8080/a-b?c=d&e=%ff"
                COMMENT "#comment"
            "##]],
        );
    }

    #[test]
    fn path() {
        check_lex(
            "<nixpkgs/pkgs> <nixpkgs/ > a/b a/ b a /b",
            expect![[r#"
                SEARCH_PATH "<nixpkgs/pkgs>"
                SPACE " "
                LT "<"
                IDENT "nixpkgs"
                SLASH "/"
                SPACE " "
                GT ">"
                SPACE " "
                PATH "a/b"
                SPACE " "
                IDENT "a"
                SLASH "/"
                SPACE " "
                IDENT "b"
                SPACE " "
                IDENT "a"
                SPACE " "
                PATH "/b"
            "#]],
        );
    }

    #[test]
    fn path_duplicated_slashes() {
        check_lex(
            "/a//b/ a/b//c /${x}//d",
            expect![[r#"
                PATH "/a//b/"
                SPACE " "
                PATH "a/b//c"
                SPACE " "
                PATH_START ""
                PATH_FRAGMENT "/"
                DOLLAR_L_CURLY "${"
                IDENT "x"
                R_CURLY "}"
                PATH_FRAGMENT "//d"
                PATH_END ""
            "#]],
        );
    }

    #[test]
    fn path_trailing_slash() {
        check_lex(
            "a/b/ /a/ ~/a/",
            expect![[r#"
                PATH "a/b/"
                SPACE " "
                PATH "/a/"
                SPACE " "
                PATH "~/a/"
            "#]],
        );
    }

    #[test]
    fn path_interpolation() {
        check_lex(
            "/${1}${2}/${3}a/${4}/a${5}a/a${6}/a/${7}",
            expect![[r#"
                PATH_START ""
                PATH_FRAGMENT "/"
                DOLLAR_L_CURLY "${"
                INT "1"
                R_CURLY "}"
                DOLLAR_L_CURLY "${"
                INT "2"
                R_CURLY "}"
                PATH_FRAGMENT "/"
                DOLLAR_L_CURLY "${"
                INT "3"
                R_CURLY "}"
                PATH_FRAGMENT "a/"
                DOLLAR_L_CURLY "${"
                INT "4"
                R_CURLY "}"
                PATH_FRAGMENT "/a"
                DOLLAR_L_CURLY "${"
                INT "5"
                R_CURLY "}"
                PATH_FRAGMENT "a/a"
                DOLLAR_L_CURLY "${"
                INT "6"
                R_CURLY "}"
                PATH_FRAGMENT "/a/"
                DOLLAR_L_CURLY "${"
                INT "7"
                R_CURLY "}"
                PATH_END ""
            "#]],
        );
        check_lex(
            "~/a/${1}/a/a/${2}a/a/a",
            expect![[r#"
                PATH_START ""
                PATH_FRAGMENT "~/a/"
                DOLLAR_L_CURLY "${"
                INT "1"
                R_CURLY "}"
                PATH_FRAGMENT "/a/a/"
                DOLLAR_L_CURLY "${"
                INT "2"
                R_CURLY "}"
                PATH_FRAGMENT "a/a/a"
                PATH_END ""
            "#]],
        );
        check_lex(
            "foo/bar/${baz}/bux/${qux}/",
            expect![[r#"
                PATH_START ""
                PATH_FRAGMENT "foo/bar/"
                DOLLAR_L_CURLY "${"
                IDENT "baz"
                R_CURLY "}"
                PATH_FRAGMENT "/bux/"
                DOLLAR_L_CURLY "${"
                IDENT "qux"
                R_CURLY "}"
                PATH_FRAGMENT "/"
                PATH_END ""
            "#]],
        );
    }

    #[test]
    fn string() {
        check_lex(
            r#"" a \" \n${ {} "${deep}er" } ""#,
            expect![[r#"
                DQUOTE "\""
                STRING_FRAGMENT " a "
                STRING_ESCAPE "\\\""
                STRING_FRAGMENT " "
                STRING_ESCAPE "\\n"
                DOLLAR_L_CURLY "${"
                SPACE " "
                L_CURLY "{"
                R_CURLY "}"
                SPACE " "
                DQUOTE "\""
                DOLLAR_L_CURLY "${"
                IDENT "deep"
                R_CURLY "}"
                STRING_FRAGMENT "er"
                DQUOTE "\""
                SPACE " "
                R_CURLY "}"
                STRING_FRAGMENT " "
                DQUOTE "\""
            "#]],
        );

        check_lex(
            r#""$${ $$\" $$x $ $""#,
            expect![[r#"
                DQUOTE "\""
                STRING_FRAGMENT "$${ $$"
                STRING_ESCAPE "\\\""
                STRING_FRAGMENT " $$x $ $"
                DQUOTE "\""
            "#]],
        );

        check_lex(
            r#""\n\_\
\ΣΣ""#,
            expect![[r#"
                DQUOTE "\""
                STRING_ESCAPE "\\n"
                STRING_ESCAPE "\\_"
                STRING_ESCAPE "\\\n"
                STRING_ESCAPE "\\Σ"
                STRING_FRAGMENT "Σ"
                DQUOTE "\""
            "#]],
        );
    }

    #[test]
    fn indent_string() {
        check_lex(
            r"'' a'''b$c''\nd''${e${{} ''f''} ''",
            expect![[r#"
                QUOTE2 "''"
                STRING_FRAGMENT " a"
                STRING_ESCAPE "'''"
                STRING_FRAGMENT "b$c"
                STRING_ESCAPE "''\\n"
                STRING_FRAGMENT "d"
                STRING_ESCAPE "''$"
                STRING_FRAGMENT "{e"
                DOLLAR_L_CURLY "${"
                L_CURLY "{"
                R_CURLY "}"
                SPACE " "
                QUOTE2 "''"
                STRING_FRAGMENT "f"
                QUOTE2 "''"
                R_CURLY "}"
                STRING_FRAGMENT " "
                QUOTE2 "''"
            "#]],
        );

        check_lex(
            r#"''$${ $$''$ $$x $ $''"#,
            expect![[r#"
                QUOTE2 "''"
                STRING_FRAGMENT "$${ $$"
                STRING_ESCAPE "''$"
                STRING_FRAGMENT " $$x $ $"
                QUOTE2 "''"
            "#]],
        );

        check_lex(
            r"''''\n''\_''\
''\ΣΣ''",
            expect![[r#"
                QUOTE2 "''"
                STRING_ESCAPE "''\\n"
                STRING_ESCAPE "''\\_"
                STRING_ESCAPE "''\\\n"
                STRING_ESCAPE "''\\Σ"
                STRING_FRAGMENT "Σ"
                QUOTE2 "''"
            "#]],
        );

        check_lex(
            "'' 'a' ''",
            expect![[r#"
                QUOTE2 "''"
                STRING_FRAGMENT " 'a' "
                QUOTE2 "''"
            "#]],
        );
    }

    #[test]
    fn or_is_keyword() {
        check_lex(
            "{ or = 1; } or",
            expect![[r#"
                L_CURLY "{"
                SPACE " "
                KW_OR "or"
                SPACE " "
                EQ "="
                SPACE " "
                INT "1"
                SEMICOLON ";"
                SPACE " "
                R_CURLY "}"
                SPACE " "
                KW_OR "or"
            "#]],
        );
    }

    #[test]
    fn error_utf8() {
        check_lex(
            "1`2£3ह4𐍈",
            expect![[r#"
                INT "1"
                ERROR "`"
                INT "2"
                ERROR "£"
                INT "3"
                ERROR "ह"
                INT "4"
                ERROR "𐍈"
            "#]],
        );
    }

    #[test]
    fn indent_string_single_quote() {
        check_lex(
            "'''${x}''",
            expect![[r#"
                QUOTE2 "''"
                STRING_FRAGMENT "'"
                DOLLAR_L_CURLY "${"
                IDENT "x"
                R_CURLY "}"
                QUOTE2 "''"
            "#]],
        );
    }

    #[test]
    fn comment() {
        check_lex(
            "/*1/*2*/3*/4",
            expect![[r#"
                COMMENT "/*1/*2*/"
                INT "3"
                STAR "*"
                PATH "/4"
            "#]],
        );
        check_lex(
            "1/**/2",
            expect![[r#"
                INT "1"
                COMMENT "/**/"
                INT "2"
            "#]],
        );
        check_lex(
            "1/*/2/**/3/***/4",
            expect![[r#"
                INT "1"
                COMMENT "/*/2/**/"
                INT "3"
                COMMENT "/***/"
                INT "4"
            "#]],
        );
    }
}
