//! Types representing the syntax of a sound change rule.

use std::{collections::HashSet, fmt};

use crate::{
    category::Ident,
    re::{Instr, Program},
    token::Token,
    utils::BooleanExpr,
};

pub mod re;

/// A single sound change rule.
pub struct Rule {
    /// The pattern to replace.
    pub search: Search,
    /// The replacement.
    pub replace: Replace,
    /// The environment in which to apply the rule.
    pub environment: BooleanExpr<Environment>,
}

impl Rule {
    pub fn matcher(&mut self) -> RuleMatcher {
        self.environment.coalesce();
        // if environment is simple, include it in the search
        if let BooleanExpr::Value(environment) = &self.environment {
            let mut search = Program::new();
            environment.before.matcher(&mut search, false);
            self.search.matcher(&mut search);
            environment.after.matcher(&mut search, false);
            RuleMatcher {
                search,
                environment: BooleanExpr::True,
            }
        } else {
            let mut search = Program::new();
            self.search.matcher(&mut search);
            let environment = self.environment.map(Environment::matcher);
            RuleMatcher {
                search,
                environment,
            }
        }
    }
}

/// The pattern to replace in a sound change rule.
pub enum Search {
    Zero,
    Pattern(Pattern),
}

impl Search {
    fn matcher(&self, program: &mut Program<re::Engine>) {
        program.push(Instr::Peek(re::Peek::ReplaceStart));
        if let Search::Pattern(pat) = self {
            pat.matcher(program, false);
        }
        program.push(Instr::Peek(re::Peek::ReplaceEnd));
    }
}

/// The replacement portion of a sound change rule.
#[derive(Clone, Debug, PartialEq)]
pub struct Replace(pub Vec<ReplaceTok>);

/// A replacement token.
#[derive(Clone, Debug, PartialEq)]
pub enum ReplaceTok {
    Token(Token),
    Category(Category),
}

/// An environment for applying a rule.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub before: Pattern,
    pub after: Pattern,
}
impl Environment {
    fn matcher(&self) -> EnvironmentMatcher {
        let mut before = Program::new();
        self.before.matcher(&mut before, true);
        let mut after = Program::new();
        self.after.matcher(&mut after, false);
        EnvironmentMatcher { before, after }
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
    fn matcher(&self, program: &mut Program<re::Engine>, reverse: bool) {
        match self {
            Pattern::Literal(tok) => program.push(Instr::Consume(re::Consume::Token(*tok))),
            Pattern::Set(tokens) => program.push(Instr::Consume(re::Consume::Set(tokens.clone()))),
            Pattern::Any => program.push(Instr::Consume(re::Consume::Any)),
            Pattern::WordBoundary => program.push(Instr::Peek(re::Peek::WordBoundary)),
            Pattern::Category(cat) => todo!(),
            Pattern::Repeat(pat, rep) => {
                let split;
                match rep {
                    Repeater::ZeroOrOne(_) => {
                        split = program.len();
                        program.push(Instr::Split(0)); // to be filled in later
                        pat.matcher(program, reverse);
                    }
                    Repeater::ZeroOrMore(_) => {
                        split = program.len();
                        program.push(Instr::Split(0)); // to be filled in later
                        pat.matcher(program, reverse);
                        program.push(Instr::Jump(split));
                    }
                    Repeater::OneOrMore(_) => {
                        let start = program.len();
                        pat.matcher(program, reverse);
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
                        pat.matcher(program, reverse);
                    }
                } else {
                    for pat in pats {
                        pat.matcher(program, reverse);
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
                        pat.matcher(program, reverse);
                        program.push(Instr::Jump(jump_instr));
                        program[split] = Instr::Split(program.len());
                    }
                    // last element doesn't have a `Split` before it, or a `Jump` after
                    last.matcher(program, reverse);
                    // end of alternates
                    program[jump_instr] = Instr::Jump(program.len());
                }
            }
        }
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
            Pattern::Category(cat) => fmt::Debug::fmt(cat, f),
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

pub struct RuleMatcher {
    search: Program<re::Engine>,
    environment: BooleanExpr<EnvironmentMatcher>,
}

struct EnvironmentMatcher {
    before: Program<re::Engine>,
    after: Program<re::Engine>,
}
