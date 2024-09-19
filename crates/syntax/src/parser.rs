use crate::ast::{AstNode, SourceFile};
use crate::lexer::LexTokens;
use crate::SyntaxKind::{self, *};
use crate::{lexer, Error, ErrorKind, SyntaxNode};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, TextRange, TextSize};

const MAX_STEPS: usize = 100_000_000;
const MAX_DEPTHS: usize = 500;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parse {
    green: GreenNode,
    errors: Vec<Error>,
}

impl Parse {
    pub fn green_node(&self) -> GreenNode {
        self.green.clone()
    }

    pub fn root(&self) -> SourceFile {
        SourceFile::cast(self.syntax_node()).expect("The entry node is SourceFile")
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

/// Parse the source of a Nix file.
///
/// # Panics
/// Panic if the source is longer than `u32::MAX`.
pub fn parse_file(src: &str) -> Parse {
    let tokens = lexer::lex(src.as_bytes());
    parse_file_tokens(src, tokens)
}

/// Parse the source of a Nix file after tokenization.
///
/// # Panics
/// Panic if the source is longer than `u32::MAX`.
pub fn parse_file_tokens(src: &str, mut tokens: LexTokens) -> Parse {
    assert!(u32::try_from(src.len()).is_ok());
    tokens.reverse();
    Parser {
        tokens,
        builder: GreenNodeBuilder::default(),
        errors: Vec::new(),
        src,
        steps: 0,
        depth: 0,
    }
    .parse()
}

struct Parser<'i> {
    tokens: lexer::LexTokens,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Error>,
    src: &'i str,
    steps: usize,
    depth: usize,
}

impl<'i> Parser<'i> {
    /// Parse the whole source file.
    fn parse(mut self) -> Parse {
        self.start_node(SOURCE_FILE);
        self.expr_function_opt();
        while self.peek_non_ws().is_some() {
            // Tolerate multiple exprs and just emit errors.
            self.error(ErrorKind::MultipleRoots);

            let prev = self.tokens.len();
            self.expr_function_opt();
            // Don't stuck.
            if self.tokens.len() == prev {
                self.bump_error();
            }
        }
        self.finish_node();

        Parse {
            green: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn error(&mut self, kind: ErrorKind) {
        let range = self
            .tokens
            .last()
            .map(|&(_, range)| range)
            .unwrap_or_else(|| TextRange::empty(TextSize::from(self.src.len() as u32)));
        self.errors.push(Error { range, kind });
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    fn start_node_at(&mut self, cp: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(cp, kind.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    /// Consume the next token, including whitespaces. Panic if there is no more token.
    fn bump(&mut self) {
        let (kind, range) = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), &self.src[range]);
    }

    /// Same with `bump`, but override the kind.
    fn bump_with_kind(&mut self, kind: SyntaxKind) {
        let (_, range) = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), &self.src[range]);
    }

    /// Consume the next token and wrap it in an `ERROR` node.
    fn bump_error(&mut self) {
        self.start_node(ERROR);
        self.bump();
        self.finish_node();
    }

    /// Peek the next token, including whitespaces.
    fn peek_full(&mut self) -> Option<(SyntaxKind, TextRange)> {
        self.steps += 1;
        assert!(self.steps < MAX_STEPS);
        self.tokens.last().copied()
    }

    /// Like `peek`, but only returns `SyntaxKind`.
    fn peek(&mut self) -> Option<SyntaxKind> {
        self.peek_full().map(|(k, _)| k)
    }

    /// Consume all following whitespaces if any, and peek the next token.
    fn peek_non_ws(&mut self) -> Option<SyntaxKind> {
        self.ws();
        self.peek()
    }

    /// Get an iterator of following non-whitespace tokens.
    fn peek_iter_non_ws(&mut self) -> impl Iterator<Item = SyntaxKind> + '_ {
        self.steps += 1;
        assert!(self.steps < MAX_STEPS);
        self.tokens
            .iter()
            .rev()
            .map(|&(k, _)| k)
            .filter(|k| !k.is_trivia())
    }

    /// Consumes all following whitespaces if any.
    fn ws(&mut self) {
        while matches!(self.peek(), Some(k) if k.is_trivia()) {
            self.bump();
        }
    }

    /// Consumes a token if the next token matches the expected one, or does nothing if not.
    /// Return whether the expected token is consumed.
    fn want(&mut self, expect: SyntaxKind) -> bool {
        if self.peek_non_ws() == Some(expect) {
            self.bump();
            true
        } else {
            self.error(ErrorKind::ExpectToken(expect));
            false
        }
    }

    /// Expect the termination of an expression by a followed `guard` token.
    /// Return whether the expected token is consumed.
    fn require_expr_end(&mut self, guard: SyntaxKind) -> bool {
        if matches!(self.peek_non_ws(), Some(k) if k == guard) {
            self.bump();
            return true;
        }
        self.error(ErrorKind::ExpectToken(guard));

        // Try to consume more expressions as recovery.
        loop {
            match self.peek_non_ws() {
                Some(k) if k == guard => {
                    self.bump();
                    return true;
                }
                Some(k) if !k.is_separator() => {
                    self.start_node(ERROR);
                    let prev = self.tokens.len();
                    self.expr_function_opt();
                    // Don't stuck!
                    if self.tokens.len() == prev {
                        self.bump();
                    }
                    self.finish_node();
                }
                _ => return false,
            }
        }
    }

    /// Function level expression (lowest priority).
    /// Maybe consume nothing.
    fn expr_function_opt(&mut self) {
        self.depth += 1;
        if self.depth >= MAX_DEPTHS {
            self.error(ErrorKind::NestTooDeep);
            self.start_node(ERROR);
            while self.peek().is_some() {
                self.bump();
            }
            self.finish_node();
            return;
        }

        // This should match SyntaxKind::can_start_expr.
        match self.peek_non_ws() {
            Some(T![assert]) => {
                self.start_node(ASSERT);
                self.bump(); // assert
                self.expr_function_opt();
                self.want(T![;]);
                self.expr_function_opt();
                self.finish_node();
            }
            Some(T![with]) => {
                self.start_node(WITH);
                self.bump(); // with
                self.expr_function_opt();
                self.want(T![;]);
                self.expr_function_opt();
                self.finish_node();
            }
            Some(T![let]) => {
                if matches!(self.peek_iter_non_ws().nth(1), Some(T!['{'])) {
                    self.expr_operator_opt();
                } else {
                    self.start_node(LET_IN);
                    self.bump(); // let
                    self.bindings_until(T![in]);
                    self.expr_function_opt();
                    self.finish_node();
                }
            }
            Some(T![rec]) => {
                if matches!(self.peek_iter_non_ws().nth(1), Some(T!['{'])) {
                    self.expr_operator_opt();
                } else {
                    self.bump_error();
                    self.error(ErrorKind::ExpectToken(T!['{']));
                }
            }
            Some(T![if]) => {
                self.start_node(IF_THEN_ELSE);
                self.bump(); // if
                self.expr_function_opt();
                // If the separator is not found, stop early.
                if self.require_expr_end(T![then]) {
                    self.expr_function_opt();
                    if self.require_expr_end(T![else]) {
                        self.expr_function_opt();
                    }
                }
                self.finish_node();
            }
            Some(T!['{']) => {
                // Recognise patterns of `LAMBDA` starting. Otherwise, it's an `ATTR_SET`.
                // - '{ ...'
                // - '{ } :'
                // - '{ } @'
                // - '{ x ,'
                // - '{ x ?'
                // - '{ x ...'
                //   This is invalid but may occur when typing.
                //
                // - '{ x } @'
                // - '{ x } :'
                //   We reject `{ x }` following tokens other than `@` and `:` as lambda.
                //   This can occur for incomplete attrsets.
                let is_lambda = {
                    let mut tok_iter = self.peek_iter_non_ws();
                    tok_iter.next(); // '{'
                    match tok_iter.next() {
                        Some(T![...]) => true,
                        Some(T!['}']) => matches!(tok_iter.next(), Some(T![:] | T![@])),
                        Some(IDENT) => match tok_iter.next() {
                            Some(T![,] | T![?] | T![...]) => true,
                            Some(T!['}']) => matches!(tok_iter.next(), Some(T![@] | T![:])),
                            _ => false,
                        },
                        _ => false,
                    }
                };

                if is_lambda {
                    self.start_node(LAMBDA);

                    self.start_node(PARAM);
                    self.pat();
                    if self.peek_non_ws() == Some(T![@]) {
                        self.bump(); // @
                        self.ws(); // Don't leave spaces in NAME.
                        self.start_node(NAME);
                        self.want(IDENT);
                        self.finish_node();
                    }
                    self.finish_node();

                    self.want(T![:]);
                    self.expr_function_opt();
                    self.finish_node();
                } else {
                    self.expr_operator_opt();
                }
            }
            Some(IDENT) => {
                // Recognise patterns of `LAMBDA` starting. Otherwise, it's a `REF`.
                // - 'x :'
                // - 'x @'
                let is_lambda = matches!(self.peek_iter_non_ws().nth(1), Some(T![:] | T![@]));

                if is_lambda {
                    self.start_node(LAMBDA);

                    self.start_node(PARAM);
                    self.start_node(NAME);
                    self.bump(); // IDENT
                    self.finish_node();
                    if self.peek_non_ws() == Some(T![@]) {
                        self.bump(); // @
                        if self.peek_non_ws() == Some(T!['{']) {
                            self.pat();
                        } else {
                            self.error(ErrorKind::ExpectToken(T!['{']));
                        }
                    }
                    self.finish_node();

                    self.want(T![:]);
                    self.expr_function_opt();
                    self.finish_node();
                } else {
                    self.expr_operator_opt();
                }
            }
            _ => self.expr_operator_opt(),
        }

        self.depth -= 1;
    }

    /// Operator level expression (low priority).
    /// Maybe consume nothing.
    fn expr_operator_opt(&mut self) {
        self.expr_bp(0);
    }

    // Pratt parser.
    // Ref: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn expr_bp(&mut self, min_bp: u8) {
        // Always consume whitespace first, even though not `allow_prefix`.
        let Some(tok) = self.peek_non_ws() else {
            self.error(ErrorKind::ExpectExpr);
            return;
        };

        let cp = self.checkpoint();

        match tok.prefix_bp() {
            Some(rbp) => {
                self.start_node(UNARY_OP);
                self.bump(); // Prefix op.
                self.expr_bp(rbp);
                self.finish_node();
            }
            _ => self.expr_select_opt(),
        }

        loop {
            let Some(tok) = self.peek_non_ws() else { break };

            if let Some(lbp) = tok.postfix_bp() {
                if lbp < min_bp {
                    break;
                }

                // Currently we have only `HAS_ATTR` as a postfix operator.
                assert_eq!(tok, T![?]);
                self.start_node_at(cp, HAS_ATTR);
                self.bump(); // `?`
                self.attrpath_opt();
                self.finish_node();
                continue;
            }

            let Some((lbp, rbp)) = tok.infix_bp() else {
                break;
            };
            if lbp == min_bp {
                self.error(ErrorKind::MultipleNoAssoc);
                break;
            }
            if lbp < min_bp {
                break;
            }

            if rbp == APPLY_RBP {
                self.start_node_at(cp, APPLY);
            } else {
                self.start_node_at(cp, BINARY_OP);
                self.bump(); // Infix op.
            }
            self.expr_bp(rbp);
            self.finish_node();
        }
    }

    /// Select expression (high priority).
    /// Maybe consume nothing.
    fn expr_select_opt(&mut self) {
        let cp = self.checkpoint();
        self.expr_atom_opt();

        if self.peek_non_ws() == Some(T![.]) {
            self.start_node_at(cp, SELECT);
            self.bump();
            self.attrpath_opt();

            if self.peek_non_ws() == Some(T![or]) {
                self.bump();
                self.expr_select_opt();
            }
            self.finish_node();

        // Yes, this is weird, but Nix parse `or` immediately after a non-select atom expression,
        // and construct a `Apply` node, with higher priority than left-associative Apply.
        // `a b or c` => `(a (b or)) c`
        } else if self.peek_non_ws() == Some(T![or]) {
            self.start_node_at(cp, APPLY);
            self.start_node(REF);
            self.bump_with_kind(SyntaxKind::IDENT);
            self.finish_node();
            self.finish_node();
        }
    }

    /// Atom level expression (highest priority).
    /// Maybe consume nothing.
    fn expr_atom_opt(&mut self) {
        // This must matches `SyntaxKind::can_start_atom_expr`.
        match self.peek_non_ws() {
            Some(IDENT) => {
                self.start_node(REF);
                self.bump(); // IDENT
                self.finish_node();
            }
            Some(k @ (INT | FLOAT | PATH | SEARCH_PATH | URI)) => {
                if k == PATH {
                    self.validate_path_fragment(false);
                }
                self.start_node(LITERAL);
                self.bump();
                self.finish_node();
            }
            Some(PATH_START) => self.path_interpolation(),
            Some(T!['"']) => self.string(STRING),
            Some(T!["''"]) => self.string(INDENT_STRING),
            Some(T!['(']) => {
                self.start_node(PAREN);
                self.bump(); // '('
                self.expr_function_opt();
                self.require_expr_end(T![')']);
                self.finish_node();
            }
            Some(T![rec] | T![let]) => {
                let cp = self.checkpoint();
                self.bump(); // rec|let

                // Only recognize 'rec {' and 'let {' if there is an '{'.
                if self.peek_non_ws() == Some(T!['{']) {
                    self.start_node_at(cp, ATTR_SET);
                    self.bump(); // '{'
                    self.bindings_until(T!['}']);
                    self.finish_node();
                // Otherwise, simply give up.
                } else {
                    self.error(ErrorKind::ExpectToken(T!['{']));
                }
            }
            Some(T!['{']) => {
                self.start_node(ATTR_SET);
                self.bump(); // '{'
                self.bindings_until(T!['}']);
                self.finish_node();
            }
            Some(T!['[']) => {
                self.start_node(LIST);
                self.bump(); // '['
                loop {
                    match self.peek_non_ws() {
                        Some(T![']']) => {
                            self.bump(); // ]
                            break;
                        }
                        // Ensure it consumes tokens in the loop!
                        Some(k) if k.can_start_atom_expr() => {
                            self.expr_select_opt();
                            continue;
                        }
                        Some(k) if !k.is_separator() => {
                            self.error(ErrorKind::ExpectElemExpr);
                            self.bump_error();
                        }
                        _ => {
                            self.error(ErrorKind::ExpectToken(T![']']));
                            break;
                        }
                    }
                }
                self.finish_node();
            }
            _ => {
                self.error(ErrorKind::ExpectExpr);
            }
        }
    }

    fn path_interpolation(&mut self) {
        assert_eq!(self.peek(), Some(PATH_START));
        self.start_node(PATH_INTERPOLATION);
        self.bump();
        // No skipping whitespace.
        while let Some(tok) = self.peek() {
            match tok {
                PATH_END => {
                    self.bump();
                    break;
                }
                PATH_FRAGMENT => {
                    let is_last = matches!(self.peek_iter_non_ws().nth(1), Some(PATH_END));
                    self.validate_path_fragment(!is_last);
                    self.bump();
                }
                T!["${"] => self.dynamic(),
                // Unpaired dynamic. Should already trigger errors.
                _ => break,
            }
        }
        self.finish_node();
    }

    /// Validate the next path fragment and emit errors about slashes.
    fn validate_path_fragment(&mut self, allow_trailing_slash: bool) {
        let Some((PATH_FRAGMENT | PATH, range)) = self.peek_full() else {
            unreachable!()
        };
        // N.B. Path tokens are ASCII-only, which are verified by the lexer.
        self.src[range]
            .as_bytes()
            .windows(2)
            .zip(1u32..)
            .filter(|(w, _)| w == b"//")
            .for_each(|(_, i)| {
                self.errors.push(Error {
                    range: TextRange::at(range.start() + TextSize::from(i), 1.into()),
                    kind: ErrorKind::PathDuplicatedSlashes,
                });
            });
        let last_char = TextRange::at(range.end() - TextSize::from(1), 1.into());
        if !allow_trailing_slash && &self.src[last_char] == "/" {
            self.errors.push(Error {
                range: last_char,
                kind: ErrorKind::PathTrailingSlash,
            });
        }
    }

    /// Always consume some tokens and make a `PAT` node.
    fn pat(&mut self) {
        assert_eq!(self.peek(), Some(T!['{']));
        self.start_node(PAT);
        self.bump(); // '{'

        loop {
            // We must consume something in this match, to make the loop progress.
            match self.peek_non_ws() {
                // Terminates with no parameter, or after `,`.
                None | Some(T!['}']) => break,

                Some(IDENT) => {
                    self.start_node(PAT_FIELD);
                    self.start_node(NAME);
                    self.bump(); // IDENT
                    self.finish_node();
                    if self.peek_non_ws() == Some(T![?]) {
                        self.bump(); // ?
                        self.expr_function_opt();
                    }
                    self.finish_node();
                }
                Some(T![...]) => {
                    self.bump(); // ...
                    if self.peek_non_ws() == Some(T!['}']) {
                        break;
                    }
                    self.error(ErrorKind::ExpectToken(T!['}']));
                    // Continue parsing parameters as recovery.
                }
                Some(k) => {
                    self.error(ErrorKind::ExpectIdent);
                    self.bump_error();
                    // Don't double error.
                    if k == T![,] {
                        continue;
                    }
                }
            }

            match self.peek_non_ws() {
                // Terminates after a parameter.
                None | Some(T!['}']) => break,
                // Separator.
                Some(T![,]) => self.bump(), // ,
                // Consume nothing here, as the previous match must consume something.
                _ => self.error(ErrorKind::ExpectToken(T![,])),
            }
        }

        self.want(T!['}']);
        self.finish_node();
    }

    /// Maybe consume tokens and maybe make many `INHERIT` or `ATTR_PATH_VALUE` nodes,
    /// and must consume the guard token or reaching EOF.
    fn bindings_until(&mut self, guard: SyntaxKind) {
        loop {
            match self.peek_non_ws() {
                None => {
                    self.error(ErrorKind::ExpectToken(guard));
                    break;
                }
                Some(k) if k == guard => {
                    self.bump(); // guard
                    break;
                }
                Some(T![inherit]) => {
                    self.start_node(INHERIT);
                    self.bump(); // inherit
                    if self.peek_non_ws() == Some(T!['(']) {
                        self.start_node(PAREN);
                        self.bump(); // '('
                        self.expr_function_opt();
                        self.require_expr_end(T![')']);
                        self.finish_node();
                    }
                    // Use lookahead for ending condition, since `;` might not be typed yet.
                    while self.peek_non_ws().map_or(false, SyntaxKind::can_start_attr) {
                        self.attr_opt(false);
                    }
                    self.want(T![;]);
                    self.finish_node();
                }
                // Ensure we always consume something in the loop!
                Some(k) if k.can_start_attr() => {
                    self.start_node(ATTR_PATH_VALUE);
                    self.attrpath_opt();
                    // If there is no `=`, we assume this binding is to be typed.
                    // Stop parsing RHS and try the next binding.
                    // ```
                    // {
                    //   b.|    # Typing...
                    //   a = 1; # Valid binding follows.
                    // }
                    // ```
                    if self.want(T![=]) {
                        self.expr_function_opt();
                    }
                    self.want(T![;]);
                    self.finish_node();
                }
                // Recover for missing attrpath. This happens when the previous binding is not
                // terminated by `;`.
                // ```
                // {
                //   a = 1   # => `a = 1 b <missing semicolon>`
                //   b = 2;  # => `= 2;` <- We are here.
                // }
                // ```
                Some(T![=]) => {
                    self.error(ErrorKind::ExpectAttr);
                    self.start_node(ATTR_PATH_VALUE);
                    self.bump(); // =
                    self.expr_function_opt();
                    self.want(T![;]);
                    self.finish_node();
                }
                // Consume tokens if it cannot start an AttrPath and is not the guard.
                // This can happen for some extra tokens (eg. unfinished exprs) in AttrSet or LetIn.
                Some(k) => {
                    self.error(ErrorKind::ExpectBinding);
                    self.start_node(ERROR);
                    // Special case for newbies placing expressions directly inside an Attrset.
                    if k.can_start_expr() {
                        self.expr_function_opt();
                    } else {
                        self.bump();
                    }
                    self.finish_node();
                }
            }
        }
    }

    /// Maybe consume tokens and always make a `ATTR_PATH` node.
    fn attrpath_opt(&mut self) {
        self.start_node(ATTR_PATH);
        self.attr_opt(true);
        while self.peek_non_ws() == Some(T![.]) {
            self.bump(); // .
            self.attr_opt(true);
        }
        self.finish_node();
    }

    /// Maybe consume tokens and always make a {IDENT,DYNAMIC,STRING} node.
    /// If `force_name` is true, an empty NAME node would be created when the next token is unexpected.
    fn attr_opt(&mut self, force_name: bool) {
        // This must matches `SyntaxKind::can_start_attr`.
        match self.peek_non_ws() {
            Some(IDENT | T![or]) => {
                self.start_node(NAME);
                self.bump_with_kind(SyntaxKind::IDENT);
                self.finish_node();
            }
            Some(T!["${"]) => self.dynamic(),
            Some(T!['"']) => self.string(STRING),
            _ => {
                self.error(ErrorKind::ExpectAttr);
                if force_name {
                    self.start_node(NAME);
                    self.finish_node();
                }
            }
        }
    }

    /// Must consume tokens and always make a `DYNAMIC` node.
    fn dynamic(&mut self) {
        assert_eq!(self.peek(), Some(T!["${"]));
        self.start_node(DYNAMIC);
        self.bump(); // "${"
        self.expr_function_opt();
        self.require_expr_end(T!['}']);
        self.finish_node();
    }

    /// Must consume tokens and always make a `STRING` or `INDENT_STRING` node.
    fn string(&mut self, node: SyntaxKind) {
        assert!(matches!(self.peek(), Some(T!['"'] | T!["''"])));
        self.start_node(node);
        self.bump(); // "|''
        loop {
            // No skipping whitespace.
            match self.peek() {
                None => {
                    self.error(ErrorKind::ExpectToken(if node == STRING {
                        T!['"']
                    } else {
                        T!["''"]
                    }));
                    break;
                }
                Some(T!['"'] | T!["''"]) => {
                    self.bump();
                    break;
                }
                Some(STRING_ESCAPE | STRING_FRAGMENT | ERROR) => self.bump(),
                Some(T!["${"]) => self.dynamic(),
                // Dynamic "${" may be unpaired, thus we might somehow
                // get unsynchronized with lexer states.
                // Here we simply go back to normal states.
                Some(_) => break,
            }
        }
        self.finish_node();
    }
}

impl SyntaxKind {
    // This must matches `Parser::attr_opt`.
    fn can_start_attr(self) -> bool {
        matches!(self, T!["${"] | T!['"'] | T![or] | IDENT)
    }

    // This must matches `Parser::expr_atom_opt`.
    fn can_start_atom_expr(self) -> bool {
        matches!(
            self,
            IDENT
                | INT
                | FLOAT
                | PATH
                | SEARCH_PATH
                | PATH_START
                | URI
                | T!['"']
                | T!["''"]
                | T!['(']
                | T![rec]
                | T![let]
                | T!['{']
                | T!['[']
        )
    }

    /// Check if a token can start an expression. Only used for error recovery.
    fn can_start_expr(self) -> bool {
        // Should match `Parser::expr_function_opt`.
        // Checked in can_start_atom_expr: T![let] | T![rec] | T!['{'] | IDENT
        self.can_start_atom_expr()
            || self.prefix_bp().is_some()
            || matches!(self, T![assert] | T![with] | T![if])
    }

    /// Whether this token is a separator in some syntax.
    /// We should stop at these tokens during error recovery.
    fn is_separator(self) -> bool {
        matches!(
            self,
            T![then] | T![else] | T![')'] | T![']'] | T!['}'] | T![=] | T![;] | T![,]
        )
    }

    #[rustfmt::skip]
    fn prefix_bp(self) -> Option<u8> {
        // See `infix_bp`.
        Some(match self {
            T![!] => 13,
            T![-] => 23,
            _ => return None,
        })
    }

    #[rustfmt::skip]
    fn postfix_bp(self) -> Option<u8> {
        // See `infix_bp`.
        Some(match self {
            T![?] => 21,
            _ => return None,
        })
    }

    #[rustfmt::skip]
    fn infix_bp(self) -> Option<(u8, u8)> {
        Some(match self {
            T![->] => (2, 1),
            T![||] => (3, 4),
            T![&&] => (5, 6),
            T![==] |
            T![!=] => (7, 7),
            T![<] |
            T![<=] |
            T![>] |
            T![>=] => (9, 9),
            T!["//"] => (12, 11),
            // Prefix `!` => 13
            T![+] |
            T![-] => (15, 16),
            T![*] |
            T![/] => (17, 18),
            T![++] => (20, 19),
            T![|>] => (21, 22),
            // Postfix `?` => 21
            // Prefix `-` => 23
            _ if self.can_start_atom_expr() => (25, 26), // APPLY
            _ => return None,
        })
    }
}

const APPLY_RBP: u8 = 26;
