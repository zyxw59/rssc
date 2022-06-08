//! Types representing the syntax of a sound change rule.

use std::fmt;

use crate::{category::Ident, token::Token, utils::BooleanExpr};

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

/// The pattern to replace in a sound change rule.
pub enum Search {
    Zero,
    Pattern(Pattern),
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

/// A regular expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    /// Matches a literal token.
    Literal(Token),
    /// Matches one of a set of literal tokens.
    Set(Vec<Token>),
    /// Matches any single segment.
    Any,
    /// Matches a word boundary.
    WordBoundary,
    /// Matches a syllable boundary.
    SyllableBoundary,
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
            Pattern::SyllableBoundary => f.write_str("$"),
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
