//! Types representing the syntax of a sound change rule.

use std::{
    collections::{BTreeMap, HashSet},
    fmt,
};

use indexmap::IndexSet;

use crate::{
    category::{Categories, Ident},
    re::{Instr, Program},
    token::{Token, TokenStr},
    utils::BooleanExpr,
};

pub mod re;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Undefined category: '{0:?}'")]
    UndefinedCategory(Ident),
}

/// A single sound change rule.
#[derive(Debug)]
pub struct Rule {
    /// The pattern to replace.
    pub search: Search,
    /// The replacement.
    pub replace: Replace,
    /// The environment in which to apply the rule.
    pub environment: BooleanExpr<Environment>,
}

impl Rule {
    pub fn matcher(&mut self, categories: &Categories) -> Result<RuleMatcher, Error> {
        self.environment.coalesce();
        // if environment is simple, include it in the search
        if let BooleanExpr::Value(environment) = &self.environment {
            let mut search = Program::floating_start();
            environment.before.matcher(&mut search, categories, false)?;
            self.search.matcher(&mut search, categories)?;
            environment.after.matcher(&mut search, categories, false)?;
            Ok(RuleMatcher {
                search,
                environment: BooleanExpr::True,
            })
        } else {
            let mut search = Program::floating_start();
            self.search.matcher(&mut search, categories)?;
            let environment = self.environment.try_map(|env| env.matcher(categories))?;
            Ok(RuleMatcher {
                search,
                environment,
            })
        }
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} > {} / {}",
            self.search, self.replace, self.environment
        )
    }
}

/// The pattern to replace in a sound change rule.
#[derive(Debug)]
pub enum Search {
    Zero,
    Pattern(Pattern),
}

impl Search {
    fn matcher(
        &self,
        program: &mut Program<re::Engine>,
        categories: &Categories,
    ) -> Result<(), Error> {
        program.push(Instr::Peek(re::Peek::ReplaceStart));
        if let Search::Pattern(pat) = self {
            pat.matcher(program, categories, false)?;
        }
        program.push(Instr::Peek(re::Peek::ReplaceEnd));
        Ok(())
    }
}

impl fmt::Display for Search {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Search::Zero => f.write_str("0"),
            Search::Pattern(pat) => fmt::Display::fmt(pat, f),
        }
    }
}

/// The replacement portion of a sound change rule.
#[derive(Clone, Debug, PartialEq)]
pub struct Replace(pub Vec<ReplaceTok>);

impl fmt::Display for Replace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            f.write_str("0")
        } else {
            for tok in &self.0 {
                fmt::Display::fmt(tok, f)?;
            }
            Ok(())
        }
    }
}

/// A replacement token.
#[derive(Clone, Debug, PartialEq)]
pub enum ReplaceTok {
    Token(Token),
    Category(Category),
}

impl fmt::Display for ReplaceTok {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReplaceTok::Token(tok) => fmt::Display::fmt(tok, f),
            ReplaceTok::Category(cat) => fmt::Display::fmt(cat, f),
        }
    }
}

/// An environment for applying a rule.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub before: Pattern,
    pub after: Pattern,
}

impl Environment {
    fn matcher(&self, categories: &Categories) -> Result<EnvironmentMatcher, Error> {
        let mut before = Program::new();
        self.before.matcher(&mut before, categories, true)?;
        let mut after = Program::new();
        self.after.matcher(&mut after, categories, false)?;
        Ok(EnvironmentMatcher { before, after })
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}_{}", self.before, self.after)
    }
}

/// A regular expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    /// Matches a literal token.
    Literal(Token),
    /// Matches one of a set of literal tokens.
    Set(HashSet<Token>),
    /// Matches any single segment.
    Any,
    /// Matches a word boundary.
    WordBoundary,
    /// Matches an element of a category, optionally associating the index of the matched element
    /// with a slot for further selection or replacement.
    Category(Category),
    /// Matches a repeating pattern
    Repeat(Box<Pattern>, Repeater),
    /// Matches a concatenation of multiple patterns
    Concat(Vec<Pattern>),
    /// Matches one of multiple patterns
    Alternate(Vec<Pattern>),
}

impl Pattern {
    fn matcher(
        &self,
        program: &mut Program<re::Engine>,
        categories: &Categories,
        reverse: bool,
    ) -> Result<(), Error> {
        match self {
            Pattern::Literal(tok) => program.push(Instr::Consume(re::Consume::Token(*tok))),
            Pattern::Set(tokens) => program.push(Instr::Consume(re::Consume::Set(tokens.clone()))),
            Pattern::Any => program.push(Instr::Consume(re::Consume::Any)),
            Pattern::WordBoundary => program.push(Instr::Peek(re::Peek::WordBoundary)),
            Pattern::Category(cat) => {
                if let Some(category) = categories.get(&cat.name) {
                    if let Some(slot) = cat.number {
                        category.capturing_matcher(program, slot, reverse);
                    } else {
                        category.non_capturing_matcher(program, reverse);
                    }
                } else {
                    return Err(Error::UndefinedCategory(cat.name.clone()));
                }
            }
            Pattern::Repeat(pat, rep) => {
                let split;
                match rep {
                    Repeater::ZeroOrOne(_) => {
                        split = program.len();
                        program.push(Instr::Split(0)); // to be filled in later
                        pat.matcher(program, categories, reverse)?;
                    }
                    Repeater::ZeroOrMore(_) => {
                        split = program.len();
                        program.push(Instr::Split(0)); // to be filled in later
                        pat.matcher(program, categories, reverse)?;
                        program.push(Instr::Jump(split));
                    }
                    Repeater::OneOrMore(_) => {
                        let start = program.len();
                        pat.matcher(program, categories, reverse)?;
                        split = program.len();
                        program.push(Instr::Split(0)); // to be filled in later
                        program.push(Instr::Jump(start));
                    }
                }
                let greedy = match rep {
                    Repeater::ZeroOrOne(greedy)
                    | Repeater::ZeroOrMore(greedy)
                    | Repeater::OneOrMore(greedy) => *greedy,
                };
                if greedy {
                    // prefer to match the pattern
                    program[split] = Instr::Split(program.len());
                } else {
                    // prefer to skip the pattern
                    program[split] = Instr::JSplit(program.len());
                }
            }
            Pattern::Concat(pats) => {
                if reverse {
                    for pat in pats.iter().rev() {
                        pat.matcher(program, categories, reverse)?;
                    }
                } else {
                    for pat in pats {
                        pat.matcher(program, categories, reverse)?;
                    }
                }
            }
            Pattern::Alternate(pats) => {
                if let Some((last, rest)) = pats.split_last() {
                    // skip over the jump instruction
                    let start_of_pattern = program.len() + 2;
                    program.push(Instr::Jump(start_of_pattern));
                    let jump_instr = program.len();
                    program.push(Instr::Jump(0)); // to be filled in later
                    for pat in rest {
                        let split = program.len();
                        program.push(Instr::Split(0)); // to be filled in later
                        pat.matcher(program, categories, reverse)?;
                        program.push(Instr::Jump(jump_instr));
                        program[split] = Instr::Split(program.len());
                    }
                    // last element doesn't have a `Split` before it, or a `Jump` after
                    last.matcher(program, categories, reverse)?;
                    // end of alternates
                    program[jump_instr] = Instr::Jump(program.len());
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Literal(tok) => write!(f, "{tok:?}"),
            Pattern::Set(v) => {
                f.write_str("[")?;
                for tok in v {
                    fmt::Debug::fmt(tok, f)?;
                }
                f.write_str("]")
            }
            Pattern::Any => f.write_str("."),
            Pattern::WordBoundary => f.write_str("#"),
            Pattern::Category(cat) => fmt::Display::fmt(cat, f),
            Pattern::Repeat(pat, rep) => write!(f, "{pat}{rep}"),
            Pattern::Concat(v) => {
                for pat in v {
                    fmt::Display::fmt(pat, f)?;
                }
                Ok(())
            }
            Pattern::Alternate(v) => {
                if let Some((first, rest)) = v.split_first() {
                    fmt::Display::fmt(first, f)?;
                    for pat in rest {
                        write!(f, "|{pat}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

/// A regular expression repetition. The boolean argument determines whether it is greedy.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Repeater {
    ZeroOrOne(bool),
    ZeroOrMore(bool),
    OneOrMore(bool),
}

impl fmt::Display for Repeater {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Repeater::ZeroOrOne(greedy) => write!(f, "?{}", if *greedy { "" } else { "?" }),
            Repeater::ZeroOrMore(greedy) => write!(f, "*{}", if *greedy { "" } else { "?" }),
            Repeater::OneOrMore(greedy) => write!(f, "+{}", if *greedy { "" } else { "?" }),
        }
    }
}

/// A representation of a category in a sound change rule.
#[derive(Clone, Debug, PartialEq)]
pub struct Category {
    /// The name of the category.
    pub name: Ident,
    /// The slot to associate the category with.
    pub number: Option<u8>,
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("{")?;
        if let Some(number) = self.number {
            write!(f, "{number}:")?;
        }
        for tok in &**self.name {
            fmt::Display::fmt(tok, f)?;
        }
        f.write_str("}")
    }
}

pub struct RuleMatcher {
    search: Program<re::Engine>,
    environment: BooleanExpr<EnvironmentMatcher>,
}

impl RuleMatcher {
    pub fn matches(&self, haystack: &TokenStr) -> Vec<re::Engine> {
        let states = self
            .search
            .exec(Default::default(), haystack.iter().copied());

        let mut by_position = BTreeMap::<(usize, usize), Vec<re::Engine>>::new();
        for state in states {
            if let Some(position) = state.replace_indices() {
                by_position.entry(position).or_default().push(state);
            }
        }

        let mut matches = Vec::new();

        for ((start, end), mut states) in by_position {
            match_boolean_expr(&self.environment, start, end, &mut states, haystack);
            matches.extend(states);
        }

        matches
    }
}

impl fmt::Display for RuleMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Search:\n{}Environment:\n{}", self.search, self.environment)
    }
}

#[derive(Debug)]
struct EnvironmentMatcher {
    before: Program<re::Engine>,
    after: Program<re::Engine>,
}

impl EnvironmentMatcher {
    fn match_around(
        &self,
        start: usize,
        end: usize,
        states: &mut Vec<re::Engine>,
        string: &TokenStr,
    ) {
        self.before
            .exec_multiple(states, string[..start].iter().rev().copied());
        self.after
            .exec_multiple(states, string[end..].iter().copied())
    }
}

impl fmt::Display for EnvironmentMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Before:\n{}After:\n{}", self.before, self.after)
    }
}

fn match_boolean_expr(
    expr: &BooleanExpr<EnvironmentMatcher>,
    start: usize,
    end: usize,
    matches: &mut Vec<re::Engine>,
    string: &TokenStr,
) {
    match expr {
        // leave `matches` intact
        BooleanExpr::True => {}
        BooleanExpr::False => matches.clear(),
        BooleanExpr::Value(env) => env.match_around(start, end, matches, string),
        BooleanExpr::And(exprs) => {
            // match each environment in series, so each match that survives has matched every
            // environment
            for expr in exprs {
                match_boolean_expr(expr, start, end, matches, string);
            }
        }
        BooleanExpr::Or(exprs) => {
            // match each environment in parallel, then take the union of all remaining matches
            let mut new_matches = IndexSet::new();
            for expr in exprs {
                let mut matches = matches.clone();
                match_boolean_expr(expr, start, end, &mut matches, string);
                new_matches.extend(matches);
            }
            *matches = new_matches.into_iter().collect();
        }
        BooleanExpr::Not(expr) => {
            matches.retain(|match_| {
                let mut matches = vec![match_.clone()];
                match_boolean_expr(expr, start, end, &mut matches, string);
                // retain only if it did not match the expression
                matches.is_empty()
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        category::{Categories, Category},
        parser::rule::Parser,
        token::{tokenizer::tokenize_with_segment_map, SegmentMap},
    };

    fn categories() -> (Categories, SegmentMap) {
        let mut segment_map = SegmentMap::new();
        const RAW_CATEGORIES: &[(&str, &[&str])] = &[
            ("V", &["a", "e", "i", "o", "u"]),
            ("P", &["p", "t", "tʃ", "k"]),
            ("N", &["m", "n", "ñ", "ŋ"]),
        ];
        let categories: Categories = RAW_CATEGORIES
            .iter()
            .map(|(name, values)| {
                let name = tokenize_with_segment_map(name, &mut segment_map);
                let values = values
                    .iter()
                    .map(|s| {
                        if s.is_empty() {
                            None
                        } else {
                            Some(tokenize_with_segment_map(s, &mut segment_map))
                        }
                    })
                    .collect();
                (name.clone(), Category::new(name, values))
            })
            .collect();
        (categories, segment_map)
    }

    #[test]
    fn simple_match() {
        let (categories, mut segment_map) = categories();
        let tokens = tokenize_with_segment_map("{V} > 0 / {P}_{P}", &mut segment_map);
        let mut rule = Parser::new(tokens).parse_rule().unwrap();

        let matcher = rule.matcher(&categories).unwrap();
        println!("{}", matcher);

        let input = tokenize_with_segment_map("potʃato", &mut segment_map);
        let matches = matcher.matches(&input);
        assert_eq!(
            matches
                .iter()
                .map(|m| m.replace_indices())
                .collect::<Vec<_>>(),
            &[Some((1, 2)), Some((1, 2)), Some((4, 5))],
        );
    }

    #[test]
    fn complex_environment() {
        let (categories, mut segment_map) = categories();
        let tokens = tokenize_with_segment_map("{V} > 0 / &(|({V}_ _{P}) !_{N})", &mut segment_map);
        let mut rule = Parser::new(tokens).parse_rule().unwrap();
        let matcher = rule.matcher(&categories).unwrap();
        println!("{}", matcher);

        let input = tokenize_with_segment_map("aonepia", &mut segment_map);
        let matches = matcher.matches(&input);
        assert_eq!(
            matches
                .iter()
                .map(|m| m.replace_indices())
                .collect::<Vec<_>>(),
            &[Some((3, 4)), Some((6, 7))],
        );
    }
}
