use crate::ast::SourceFile;
use crate::SyntaxKind::{self, *};
use crate::{lexer, Error, ErrorKind, SyntaxNode};
use rowan::ast::AstNode;
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, TextRange, TextSize};

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
        SourceFile::cast(self.syntax_node()).unwrap()
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

pub fn parse_file(src: &str) -> Parse {
    assert!(src.len() < u32::MAX as usize);
    let mut tokens = lexer::lex(src.as_bytes());
    tokens.reverse();
    Parser {
        tokens,
        builder: GreenNodeBuilder::default(),
        errors: Vec::new(),
        src,
    }
    .parse()
}

struct Parser<'i> {
    tokens: lexer::LexTokens,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Error>,
    src: &'i str,
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
                self.bump();
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
        self.builder.finish_node()
    }

    /// Consume the next token, including whitespaces. Panic if there is no more token.
    fn bump(&mut self) {
        let (kind, range) = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), &self.src[range]);
    }

    /// Peek the next token, including whitespaces.
    fn peek(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|&(k, _)| k)
    }

    /// Consume all following whitespaces if any, and peek the next token.
    fn peek_non_ws(&mut self) -> Option<SyntaxKind> {
        self.ws();
        self.peek()
    }

    /// Get an iterator of following non-whitespace tokens.
    fn iter_non_ws(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        self.tokens
            .iter()
            .rev()
            .map(|&(k, _)| k)
            .filter(|k| !k.is_whitespace())
    }

    /// Consumes all following whitespaces if any.
    fn ws(&mut self) {
        while matches!(self.peek(), Some(k) if k.is_whitespace()) {
            self.bump();
        }
    }

    /// Consumes a token if the next token matches the expected one, or does nothing if not.
    fn want(&mut self, expect: SyntaxKind) {
        if self.peek_non_ws() == Some(expect) {
            self.bump();
        } else {
            self.error(ErrorKind::MissingToken(expect));
        }
    }

    /// Consumes tokens until it matches the expected one.
    fn require(&mut self, expect: SyntaxKind) {
        loop {
            match self.peek_non_ws() {
                Some(k) if k == expect => {
                    self.bump();
                    break;
                }
                Some(_) => {
                    self.error(ErrorKind::UnexpectedToken);
                    self.bump();
                }
                None => {
                    self.error(ErrorKind::MissingToken(expect));
                    break;
                }
            }
        }
    }

    /// Function level expression (lowest priority).
    /// Maybe consume nothing.
    fn expr_function_opt(&mut self) {
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
                self.finish_node()
            }
            Some(T![let]) => {
                if matches!(self.iter_non_ws().nth(1), Some(T!['{'])) {
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
                if matches!(self.iter_non_ws().nth(1), Some(T!['{'])) {
                    self.expr_operator_opt()
                } else {
                    self.error(ErrorKind::UnexpectedToken);
                    self.bump();
                }
            }
            Some(T![if]) => {
                self.start_node(IF_THEN_ELSE);
                self.bump(); // if
                self.expr_function_opt();
                self.require(T![then]);
                self.expr_function_opt();
                self.require(T![else]);
                self.expr_function_opt();
                self.finish_node();
            }
            Some(T!['{']) => {
                // Recognise patterns of LAMBDA starting. Otherwise, it's an ATTR_SET.
                // - '{ ...'
                // - '{ } :'
                // - '{ } @'
                // - '{ x ,'
                // - '{ x ?'
                // - '{ x }'
                // - '{ x ...' (This is invalid but may occur when typing.)
                let is_lambda = {
                    let mut tok_iter = self.iter_non_ws();
                    tok_iter.next(); // '{'
                    match tok_iter.next() {
                        Some(T![...]) => true,
                        Some(T!['}']) => matches!(tok_iter.next(), Some(T![:] | T![@])),
                        Some(IDENT) => {
                            matches!(tok_iter.next(), Some(T![,] | T![?] | T!['}'] | T![...]))
                        }
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
                // Recognise patterns of LAMBDA starting. Otherwise, it's an REF.
                // - 'x :'
                // - 'x @'
                let is_lambda = matches!(self.iter_non_ws().nth(1), Some(T![:] | T![@]));

                if is_lambda {
                    self.start_node(LAMBDA);

                    self.start_node(PARAM);
                    self.start_node(NAME);
                    self.bump(); // IDENT
                    self.finish_node();
                    if self.peek_non_ws() == Some(T![@]) {
                        self.bump();
                        if self.peek_non_ws() == Some(T!['{']) {
                            self.pat();
                        } else {
                            self.error(ErrorKind::MissingToken(T!['{']));
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
    }

    /// Operator level expression (low priority).
    /// Maybe consume nothing.
    fn expr_operator_opt(&mut self) {
        self.op_imply();
    }

    fn op_left(&mut self, mut next: impl FnMut(&mut Self), ops: &[SyntaxKind]) {
        let cp = self.checkpoint();
        next(self);
        while matches!(self.peek_non_ws(), Some(k) if ops.contains(&k)) {
            self.start_node_at(cp, BINARY_OP);
            self.bump();
            next(self);
            self.finish_node();
        }
    }
    fn op_right(&mut self, mut next: impl FnMut(&mut Self), ops: &[SyntaxKind]) {
        let mut cp = self.checkpoint();
        next(self);
        let mut node_cnt = 0usize;
        while matches!(self.peek_non_ws(), Some(k) if ops.contains(&k)) {
            node_cnt += 1;
            self.start_node_at(cp, BINARY_OP);
            self.bump();
            cp = self.checkpoint();
            next(self);
        }
        for _ in 0..node_cnt {
            self.finish_node();
        }
    }
    fn op_no_assoc(&mut self, mut next: impl FnMut(&mut Self), ops: &[SyntaxKind]) {
        let cp = self.checkpoint();
        next(self);
        while matches!(self.peek_non_ws(), Some(k) if ops.contains(&k)) {
            self.start_node_at(cp, BINARY_OP);
            self.bump();
            next(self);

            // Tolerate incorrect usage of no associative operators, and just mark them.
            while matches!(self.peek_non_ws(), Some(k) if ops.contains(&k)) {
                self.error(ErrorKind::MultipleNoAssoc);
                self.bump();
                next(self);
            }
            self.finish_node()
        }
    }
    fn op_unary(&mut self, mut next: impl FnMut(&mut Self), tok: SyntaxKind) {
        let mut node_cnt = 0usize;
        while self.peek_non_ws() == Some(tok) {
            self.start_node(UNARY_OP);
            self.bump();
            node_cnt += 1;
        }
        next(self);
        for _ in 0..node_cnt {
            self.finish_node();
        }
    }

    fn op_imply(&mut self) {
        self.op_right(Self::op_or, &[T![->]]);
    }
    fn op_or(&mut self) {
        self.op_left(Self::op_and, &[T![||]]);
    }
    fn op_and(&mut self) {
        self.op_left(Self::op_eq, &[T![&&]]);
    }
    fn op_eq(&mut self) {
        self.op_no_assoc(Self::op_cmp, &[T![==], T![!=]]);
    }
    fn op_cmp(&mut self) {
        self.op_no_assoc(Self::op_update, &[T![<], T![>], T![<=], T![>=]]);
    }
    fn op_update(&mut self) {
        self.op_right(Self::op_not, &[T!["//"]]);
    }
    fn op_not(&mut self) {
        self.op_unary(Self::op_add, T![!]);
    }
    fn op_add(&mut self) {
        self.op_left(Self::op_mul, &[T![+], T![-]]);
    }
    fn op_mul(&mut self) {
        self.op_left(Self::op_concat, &[T![*], T![/]]);
    }
    fn op_concat(&mut self) {
        self.op_right(Self::op_has_attr, &[T![++]]);
    }
    fn op_has_attr(&mut self) {
        let cp = self.checkpoint();
        self.op_neg();
        while self.peek_non_ws() == Some(T![?]) {
            self.start_node_at(cp, HAS_ATTR);
            self.bump(); // ?
            self.attrpath_opt();
            self.finish_node();
        }
    }
    fn op_neg(&mut self) {
        self.op_unary(Self::op_app, T![-]);
    }
    fn op_app(&mut self) {
        let next = Self::expr_select_opt;
        let cp = self.checkpoint();
        next(self);
        while matches!(self.peek_non_ws(), Some(k) if k.can_start_atom_expr()) {
            self.start_node_at(cp, APPLY);
            next(self);
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

        // Yes, this is wierd, but Nix parse `or` immediately after a non-select atom expression,
        // and construct a Apply node, with higher priority than left-associative Apply.
        // `a b or c` => `(a (b or)) c`
        } else if self.peek_non_ws() == Some(T![or]) {
            self.start_node_at(cp, APPLY);
            self.start_node(REF);
            self.bump(); // or
            self.finish_node();
            self.finish_node();
        }
    }

    /// Atom level expression (highest priority).
    /// Maybe consume nothing.
    fn expr_atom_opt(&mut self) {
        // This must matches SyntaxKind::can_start_atom_expr.
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
                self.require(T![')']);
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
                // Otherwise, simply skip one token to help better recovery.
                } else {
                    self.error(ErrorKind::UnexpectedToken);
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
                            self.bump();
                            break;
                        }
                        // Ensure it consumes tokens in the loop!
                        Some(k) if k.can_start_atom_expr() => {
                            self.expr_select_opt();
                            continue;
                        }
                        Some(_) => {
                            self.error(ErrorKind::UnexpectedToken);
                            self.bump();
                        }
                        None => {
                            self.error(ErrorKind::MissingToken(T![']']));
                            break;
                        }
                    }
                }
                self.finish_node();
            }
            _ => {
                self.error(ErrorKind::UnexpectedToken);
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
                    let is_last =
                        matches!(self.tokens.get(self.tokens.len() - 2), Some((PATH_END, _)));
                    self.validate_path_fragment(!is_last);
                    self.bump();
                }
                T!["${"] => self.dynamic(),
                _ => unreachable!(),
            }
        }
        self.finish_node();
    }

    /// Validate the next path fragment and emit errors about slashes.
    fn validate_path_fragment(&mut self, allow_trailing_slash: bool) {
        let range = match self.tokens.last() {
            Some((PATH_FRAGMENT | PATH, range)) => *range,
            _ => unreachable!(),
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

    /// Always consume some tokens and make a PAT node.
    fn pat(&mut self) {
        assert_eq!(self.peek(), Some(T!['{']));
        self.start_node(PAT);
        self.bump(); // '{'
        let mut expect_delimiter = false;
        loop {
            if self.peek_non_ws() == Some(T![,]) {
                if expect_delimiter {
                    self.bump();
                } else {
                    self.error(ErrorKind::UnexpectedToken);
                }
                continue;
            }

            match self.peek_non_ws() {
                None => break,
                Some(T!['}']) => {
                    self.bump(); // '}'
                    break;
                }
                Some(T![...]) => {
                    self.bump(); // ...
                    if self.peek_non_ws() == Some(T!['}']) {
                        self.bump();
                        break;
                    }
                    self.error(ErrorKind::UnexpectedToken);
                    expect_delimiter = false;
                }
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
                    expect_delimiter = true;
                }
                Some(_) => {
                    self.error(ErrorKind::UnexpectedToken);
                    self.bump();
                    expect_delimiter = false;
                }
            }
        }
        self.finish_node();
    }

    /// Maybe consume tokens and maybe make many INHERIT or ATTR_PATH_VALUE nodes,
    /// and must consume the guard token or reaching EOF.
    fn bindings_until(&mut self, guard: SyntaxKind) {
        loop {
            match self.peek_non_ws() {
                None => {
                    self.error(ErrorKind::MissingToken(guard));
                    break;
                }
                Some(k) if k == guard => {
                    self.bump();
                    break;
                }
                Some(T![inherit]) => {
                    self.start_node(INHERIT);
                    self.bump(); // inherit
                    if self.peek_non_ws() == Some(T!['(']) {
                        self.start_node(PAREN);
                        self.bump(); // '('
                        self.expr_function_opt();
                        self.require(T![')']);
                        self.finish_node();
                    }
                    // Use lookahead for ending condition, since `;` might not be typed yet.
                    while self.peek_non_ws().map_or(false, SyntaxKind::can_start_attr) {
                        self.attr_opt();
                    }
                    self.want(T![;]);
                    self.finish_node()
                }
                // Ensure we always consume somthing in the loop!
                Some(k) if k.can_start_attr() => {
                    self.start_node(ATTR_PATH_VALUE);
                    self.attrpath_opt();
                    self.want(T![=]);
                    self.expr_function_opt();
                    self.want(T![;]);
                    self.finish_node();
                }
                // Consume one token if it cannot start an AttrPath and is not the guard.
                // This can happen for some extra tokens (eg. unfinished exprs) in AttrSet or LetIn.
                Some(_) => {
                    self.error(ErrorKind::UnexpectedToken);
                    self.bump();
                }
            }
        }
    }

    /// Maybe consume tokens and always make a ATTR_PATH node.
    fn attrpath_opt(&mut self) {
        self.start_node(ATTR_PATH);
        self.attr_opt();
        while self.peek_non_ws() == Some(T![.]) {
            self.bump(); // .
            self.attr_opt();
        }
        self.finish_node();
    }

    /// Maybe consume tokens and always make a {IDENT,DYNAMIC,STRING} node.
    fn attr_opt(&mut self) {
        // This must matches SyntaxKind::can_start_attr.
        match self.peek_non_ws() {
            Some(IDENT | T![or]) => {
                self.start_node(NAME);
                self.bump();
                self.finish_node();
            }
            Some(T!["${"]) => self.dynamic(),
            Some(T!['"']) => self.string(STRING),
            _ => self.error(ErrorKind::MissingAttr),
        }
    }

    /// Must consume tokens and always make a DYNAMIC node.
    fn dynamic(&mut self) {
        assert_eq!(self.peek(), Some(T!["${"]));
        self.start_node(DYNAMIC);
        self.bump(); // "${"
        self.expr_function_opt();
        self.require(T!['}']);
        self.finish_node();
    }

    /// Must consume tokens and always make a STRING or INDENT_STRING node.
    fn string(&mut self, node: SyntaxKind) {
        assert!(matches!(self.peek(), Some(T!['"'] | T!["''"])));
        self.start_node(node);
        self.bump(); // "|''
        loop {
            // No skipping whitespace.
            match self.peek() {
                None => {
                    self.error(ErrorKind::MissingToken(if node == STRING {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    Add,
    Sub,
    Mul,
    Div,
    Concat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOpKind {
    Not,
    Neg,
}

impl SyntaxKind {
    fn is_whitespace(self) -> bool {
        matches!(self, COMMENT | SPACE)
    }

    // This must matches Parser::attr_opt.
    fn can_start_attr(self) -> bool {
        matches!(self, T!["${"] | T!['"'] | T![or] | IDENT)
    }

    // This must matches Parser::expr_atom_opt.
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
}
