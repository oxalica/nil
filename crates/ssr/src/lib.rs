use std::str::FromStr;

use anyhow::{bail, ensure, Context, Result};
use indexmap::{IndexMap, IndexSet};
use smol_str::SmolStr;
use syntax::ast::{self, AstNode};
use syntax::rowan::WalkEvent;
use syntax::{NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Pattern {
    raw: RawPattern,
    names: IndexSet<SmolStr>,
}

impl Pattern {
    pub fn parse(pattern: &str) -> Result<Self> {
        let raw = pattern.parse::<RawPattern>()?;
        let names = raw.placeholders.values().cloned().collect::<IndexSet<_>>();
        ensure!(
            raw.placeholders.len() == names.len(),
            "duplicated placeholders"
        );
        Ok(Self { raw, names })
    }

    pub fn find_iter(&self, input: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> + '_ {
        input.descendants().filter(|n| self.matches(n))
    }

    pub fn replace_edits(
        &self,
        template: &Template,
        input: &SyntaxNode,
    ) -> Vec<(TextRange, String)> {
        let mut edits = Vec::new();
        let mut captures = vec![None; self.raw.placeholders.len()];
        let mut iter = input.preorder();
        while let Some(event) = iter.next() {
            let WalkEvent::Enter(n) = event else { continue };
            if self.captures(&n, &mut captures) {
                iter.skip_subtree();

                let mut replacee = String::new();
                self.substitute_node(&template.raw.node, template, &captures, &mut replacee);
                edits.push((n.text_range(), replacee));
            }
        }
        edits
    }

    pub fn replace(&self, src: &str, template: &Template, input: &SyntaxNode) -> String {
        let edits = self.replace_edits(template, input);
        debug_assert!(
            edits.windows(2).all(|w| w[0].0.end() <= w[1].0.start()),
            "no overlapping",
        );

        // TODO: Dedup this with `TextEdit::apply`.
        let mut ret = String::new();
        for (prev_end, (range, ins)) in std::iter::once(0.into())
            .chain(edits.iter().map(|(range, _)| range.end()))
            .zip(&edits)
        {
            ret.push_str(&src[TextRange::new(prev_end, range.start())]);
            ret.push_str(ins);
        }
        if let Some((range, _)) = edits.last() {
            ret.push_str(&src[range.end().into()..]);
        }
        ret
    }

    fn substitute_node(
        &self,
        template_node: &SyntaxNode,
        template: &Template,
        captures: &[Option<SyntaxNode>],
        ret: &mut String,
    ) {
        for nt in template_node.children_with_tokens() {
            let range = nt.text_range();
            if let Some(templ_idx) = template.raw.placeholders.get_index_of(&range) {
                // NB. `templ_idx` is the placeholder index of the template, while
                // `captures` expects the index of the pattern.
                let pat_idx = self
                    .names
                    .get_index_of(&template.raw.placeholders[templ_idx])
                    .unwrap();

                let replacee = captures[pat_idx].clone().unwrap();
                let replacee_text = replacee.text();

                // If the old parent cannot safely contain the replacee, wrap it in parentheses.
                let parent_expr = nt.parent().and_then(ast::Expr::cast);
                let replacee_expr = ast::Expr::cast(replacee.clone());
                let need_paren = matches!((parent_expr, replacee_expr), (Some(outer), Some(inner)) if !outer.contains_without_paren(&inner));

                if need_paren {
                    ret.push('(');
                }
                replacee_text.for_each_chunk(|s| ret.push_str(s));
                if need_paren {
                    ret.push(')');
                }

                continue;
            }
            match nt {
                NodeOrToken::Token(t) => ret.push_str(t.text()),
                NodeOrToken::Node(n) => self.substitute_node(&n, template, captures, ret),
            }
        }
    }

    fn matches(&self, input: &SyntaxNode) -> bool {
        self.matches_node(&self.raw.node, input, &mut |_, _| {})
    }

    fn captures(&self, input: &SyntaxNode, captures: &mut [Option<SyntaxNode>]) -> bool {
        self.matches_node(&self.raw.node, input, &mut |i, n| {
            captures[i] = Some(n);
        })
    }

    fn matches_node(
        &self,
        pat: &SyntaxNode,
        input: &SyntaxNode,
        on_placeholder: &mut impl FnMut(usize, SyntaxNode),
    ) -> bool {
        if pat.kind() != input.kind() {
            return false;
        }

        fn no_trivia(nt: &NodeOrToken<SyntaxNode, SyntaxToken>) -> bool {
            !matches!(nt.as_token(), Some(tok) if tok.kind().is_trivia())
        }

        let mut pat_iter = pat.children_with_tokens().filter(no_trivia);
        let mut input_iter = input.children_with_tokens().filter(no_trivia);
        loop {
            match (pat_iter.next(), input_iter.next()) {
                (None, None) => break true,
                (Some(NodeOrToken::Node(pat_node)), Some(NodeOrToken::Node(input_node))) => {
                    // Placeholders match any expressions.
                    if let Some(i) = self.raw.placeholders.get_index_of(&pat_node.text_range()) {
                        if ast::Expr::can_cast(input_node.kind()) {
                            on_placeholder(i, input_node);
                            continue;
                        }
                    } else if self.matches_node(&pat_node, &input_node, on_placeholder) {
                        continue;
                    }
                    break false;
                }
                (Some(NodeOrToken::Token(lhs)), Some(NodeOrToken::Token(rhs)))
                    if lhs.text() == rhs.text() => {}
                _ => break false,
            }
        }
    }
}

#[derive(Debug)]
pub struct Template {
    raw: RawPattern,
}

impl Template {
    pub fn parse(templ: &str, pat: &Pattern) -> Result<Self> {
        let raw = templ.parse::<RawPattern>()?;
        if let Some(name) = raw
            .placeholders
            .values()
            .find(|&name| !pat.names.contains(name))
        {
            bail!("missing placeholder ${name} from source pattern");
        }
        Ok(Self { raw })
    }
}

#[derive(Debug)]
struct RawPattern {
    placeholders: IndexMap<TextRange, SmolStr>,
    node: SyntaxNode,
}

impl FromStr for RawPattern {
    type Err = anyhow::Error;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        let mut placeholders = IndexMap::new();

        let mut iter = syntax::lexer::lex(src.as_bytes()).into_iter();
        let mut tokens = Vec::with_capacity(iter.size_hint().0);
        while let Some((kind, range)) = iter.next() {
            if kind == SyntaxKind::ERROR && &src[range] == "$" {
                let (_, ident_range) = iter
                    .next()
                    .filter(|(kind, _)| *kind == SyntaxKind::IDENT)
                    .context("missing placeholder name")?;
                let name = &src[ident_range];

                // Union two ranges and create a identifier token covering both of them.
                // Since `rowan` parser maintains sizes itself using string length, we cannot
                // create virtual tokens with mismatched length.
                let range = range.cover(ident_range);
                assert!(placeholders.insert(range, name.into()).is_none());
                tokens.push((SyntaxKind::IDENT, range));
            } else {
                tokens.push((kind, range));
            }
        }

        let parse = syntax::parser::parse_file_tokens(src, tokens);
        if let Some(err) = parse.errors().first() {
            bail!("syntax error: {}", err);
        }
        Ok(Self {
            placeholders,
            node: parse.root().expr().unwrap().syntax().clone(),
        })
    }
}
