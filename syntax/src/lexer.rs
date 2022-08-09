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

regex_dfa! {
    PATH_TOKEN_DFA, PATH_TOKEN_MAP {
        DOLLAR_L_CURLY = r"\$\{",
        PATH_FRAGMENT = r"[/a-zA-Z0-9._+-]+",
    }
}

pub type LexTokens = Vec<(SyntaxKind, TextRange)>;

pub fn lex(src: &[u8]) -> LexTokens {
    let total_len = TextSize::try_from(src.len()).expect("Length overflow");

    let default_ctx = (&*DEFAULT_TOKEN_DFA, DEFAULT_TOKEN_MAP);
    let string_ctx = (&*STRING_TOKEN_DFA, STRING_TOKEN_MAP);
    let indent_string_ctx = (&*INDENT_STRING_TOKEN_DFA, INDENT_STRING_TOKEN_MAP);
    let path_ctx = (&*PATH_TOKEN_DFA, PATH_TOKEN_MAP);

    let mut out = Vec::new();
    let mut ctxs = Vec::new();

    let mut offset = TextSize::from(0);
    while offset != total_len {
        let (dfa, map) = ctxs.last().copied().unwrap_or(default_ctx);

        let rest = &src[usize::from(offset)..];
        let (mut tok, mut len) = match dfa.find_leftmost_fwd(rest).expect("No quit byte") {
            // The length of src is checked before.
            Some(m) => (
                map[m.pattern().as_usize()],
                TextSize::from(m.offset() as u32),
            ),
            None if ptr::eq(dfa, path_ctx.0) => {
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
            PATH_START => {
                len -= TextSize::of("${");
                ctxs.push(path_ctx);
                out.push((PATH_START, TextRange::empty(offset)));
                tok = PATH_FRAGMENT;
            }
            _ => {}
        }

        out.push((tok, TextRange::at(offset, len)));
        offset += len;
    }

    if matches!(ctxs.last(), Some((dfa, _)) if ptr::eq(*dfa, path_ctx.0)) {
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
        dbg!(&toks);
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
                STRING_FRAGMENT " $$x $ "
                STRING_FRAGMENT "$"
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

    #[test]
    fn error_utf8() {
        check_lex(
            "lݟ",
            expect![[r#"
                IDENT "l"
                ERROR "ݟ"
            "#]],
        )
    }
}
