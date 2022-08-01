use crate::SyntaxKind::{self, *};
use once_cell::sync::Lazy;
use regex_automata::dfa::{dense, Automaton};
use regex_automata::nfa::thompson;
use regex_automata::SyntaxConfig;
use rowan::{TextRange, TextSize};
use std::ptr;

type Dfa = dense::DFA<Vec<u32>>;

fn build_dfa(pats: &[&str]) -> Dfa {
    dense::Builder::new()
        .configure(dense::Config::new().minimize(false).anchored(true))
        .syntax(SyntaxConfig::new().unicode(false).utf8(false))
        .thompson(thompson::Config::new().utf8(false))
        .build_many(pats)
        .unwrap()
}

macro_rules! regex_dfa {
    ($static:ident, $map:ident { $($tok:path = $regex:literal,)* }) => {
        static $map: &[SyntaxKind] = &[$($tok),*];

        static $static: Lazy<Dfa> = Lazy::new(|| {
            build_dfa(&[$($regex),*])
        });
    };
}

regex_dfa! {
    DEFAULT_TOKEN_DFA, DEFAULT_TOKEN_MAP {
        // The order matters!
        SPACE = r"[ \r\n\t]+",
        COMMENT = r"#.*|/\*([^*]|\*[^/])*\*/",
        ABSOLUTE_PATH = r"(/[a-zA-Z0-9._+-]+)+",
        RELATIVE_PATH = r"[a-zA-Z0-9._+-]+(/[a-zA-Z0-9._+-]+)+",
        HOME_PATH = r"~(/[a-zA-Z0-9._+-]+)+",
        SEARCH_PATH = r"<[a-zA-Z0-9._+-]+(/[a-zA-Z0-9._+-]+)*>",
        FLOAT = r"(\d+\.\d*|\.\d+)([Ee][+-]?\d+)?",
        INT = r"\d+",
        URI = r"[a-zA-Z][a-zA-Z0-9.+-]*:[a-zA-Z0-9%/?:@&=+$,_.!~*'-]+",
        IDENT = r"[a-zA-Z_][a-zA-Z0-9_'-]*",

        DQUOTE = "\"",
        QUOTE2 = r"''",

        DOT3 = r"\.\.\.",
        MINUS_GT = r"->",
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
    STRING_TOKEN_DFA, STRING_TOKEN_MAP {
        // The order matters!
        DQUOTE = r#"""#,
        // Yes, we parse one UTF-8 encoded char here, to avoid break into code units.
        // We can assume the input is already a valid UTF-8 string.
        STRING_ESCAPE = r#"\\([\x00-\x7F]|[\x80-\xFF][\x80-\xBF]*)"#,
        DOLLAR_L_CURLY = r"\$\{",
        STRING_FRAGMENT = r#"([^"$\\]|\$[^{"\\])+"#,
        // For '$' before ending.
        STRING_FRAGMENT = r#"\$"#,
    }
}

regex_dfa! {
    INDENT_STRING_TOKEN_DFA, INDENT_STRING_TOKEN_MAP {
        // The order matters!
        // See comments in STRING_TOKEN_DFA's STRING_ESCAPE.
        STRING_ESCAPE = r#"''\\([\x00-\x7F]|[\x80-\xFF][\x80-\xBF]*)|''\$|'''"#,
        QUOTE2 = r"''",
        DOLLAR_L_CURLY = r"\$\{",
        STRING_FRAGMENT = r"([^'$]|\$[^{'])+",
        // For '$' before ending.
        STRING_FRAGMENT = r"\$",
    }
}

pub type LexTokens = Vec<(SyntaxKind, TextRange)>;

pub fn lex(src: &[u8]) -> LexTokens {
    let total_len = TextSize::try_from(src.len()).expect("Length overflow");

    let default_ctx = (&*DEFAULT_TOKEN_DFA, DEFAULT_TOKEN_MAP);
    let string_ctx = (&*STRING_TOKEN_DFA, STRING_TOKEN_MAP);
    let indent_string_ctx = (&*INDENT_STRING_TOKEN_DFA, INDENT_STRING_TOKEN_MAP);

    let mut out = Vec::new();
    let mut ctxs = Vec::new();

    let mut offset = TextSize::from(0);
    while offset != total_len {
        let (dfa, map) = ctxs.last().copied().unwrap_or(default_ctx);

        let rest = &src[usize::from(offset)..];
        let (mut tok, len) = match dfa.find_leftmost_fwd(rest).expect("No quit byte") {
            // The length of src is checked before.
            Some(m) => (
                map[m.pattern().as_usize()],
                TextSize::from(m.offset() as u32),
            ),
            None => {
                out.push((ERROR, TextRange::at(offset, 1.into())));
                offset += TextSize::from(1);
                continue;
            }
        };

        match tok {
            T!['"'] | T!["''"] if !ptr::eq(dfa, default_ctx.0) => {
                ctxs.pop();
            }
            T!['"'] => ctxs.push(string_ctx),
            T!["''"] => ctxs.push(indent_string_ctx),
            T!['{'] | T!["${"] => ctxs.push(default_ctx),
            T!['}'] => {
                ctxs.pop();
            }
            IDENT => {
                tok = match &rest[..usize::from(len)] {
                    b"assert" => T![assert],
                    b"else" => T![else],
                    b"if" => T![if],
                    b"in" => T![in],
                    b"inherit" => T![inherit],
                    b"let" => T![let],
                    b"or" => T![or],
                    b"rec" => T![rec],
                    b"then" => T![then],
                    b"with" => T![with],
                    _ => IDENT,
                };
            }
            _ => {}
        }

        out.push((tok, TextRange::at(offset, len)));
        offset += len;
    }

    out
}

#[cfg(test)]
mod test {
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
                RELATIVE_PATH "a/b"
                SPACE " "
                IDENT "a"
                SLASH "/"
                SPACE " "
                IDENT "b"
                SPACE " "
                IDENT "a"
                SPACE " "
                ABSOLUTE_PATH "/b"
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
                STRING_FRAGMENT " $$x $ "
                STRING_FRAGMENT "$"
                DQUOTE "\""
            "#]],
        );

        // '𐍈' U+10348 b"\xF0\x90\x8D\x88"
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
            r#"'' a'''b$c''\nd''${e${{} ''f''} ''"#,
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
                STRING_FRAGMENT " $$x $ "
                STRING_FRAGMENT "$"
                QUOTE2 "''"
            "#]],
        );

        // '𐍈' U+10348 b"\xF0\x90\x8D\x88"
        check_lex(
            r#"''''\n''\_''\
''\ΣΣ''"#,
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
}
