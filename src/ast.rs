use std::fmt;

use token::Token;

/// A single sound change rule.
pub struct Rule {
    /// The pattern to replace.
    pub from: Pattern,
    /// The replacement.
    pub replace: Replace,
    /// The environment in which to apply the rule.
    pub environment: Environment,
}

/// The replacement portion of a sound change rule.
pub struct Replace(Vec<ReplaceTok>);

/// A replacement token.
pub enum ReplaceTok {
    Token(Token),
    Category(Category),
}

/// An environment for applying a rule.
pub struct Environment {
//    before: Pattern,
//    after: Pattern,
//    unbefore: Pattern,
//    unafter: Pattern,
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
    /// Matches a concatenation of two patterns
    Concat(Vec<Pattern>),
    /// Matches one of multiple patterns
    Alternate(Vec<Pattern>),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Pattern::*;
        match *self {
            Literal(tok) => write!(f, "{:?}", tok),
            Set(ref v) => write!(
                f,
                "[{}]",
                v.iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<_>>()
                    .join("")
            ),
            Any => write!(f, "."),
            WordBoundary => write!(f, "#"),
            SyllableBoundary => write!(f, "$"),
            Category(ref cat) => write!(f, "{:?}", cat),
            Repeat(ref pat, ref rep) => write!(f, "{}{}", pat, rep),
            Concat(ref v) => write!(
                f,
                "{}",
                v.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join("")
            ),
            Alternate(ref v) => write!(
                f,
                "{}",
                v.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join("|")
            ),
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
        use self::Repeater::*;
        match *self {
            ZeroOrOne(greedy) => write!(f, "?{}", if greedy { "" } else { "?" }),
            ZeroOrMore(greedy) => write!(f, "*{}", if greedy { "" } else { "?" }),
            OneOrMore(greedy) => write!(f, "+{}", if greedy { "" } else { "?" }),
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

/// A category name.
pub type Ident = Vec<Token>;
