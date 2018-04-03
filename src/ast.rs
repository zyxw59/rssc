use std::fmt;
use std::str::FromStr;
use regex::Regex;

pub struct Rule {
//    search: Search,
//    replace: Pattern,
}

pub struct Search {
//    from: Pattern,
//    environment: Environment,
}

pub struct Environment {
//    before: Pattern,
//    after: Pattern,
//    unbefore: Pattern,
//    unafter: Pattern,
}

/// Almost a regular expression
#[derive(Clone, Debug)]
pub enum Pattern {
    /// Matches an empty string, i.e. zero segments.
    Empty,
    /// Matches a literal string of segments.
    Literal(String),
    /// Matches any single segment.
    Any,
    /// Matches a word boundary.
    WordBoundary,
    /// Matches an element of a category, optionally associating the index of the matched element
    /// with a slot for further selection or replacement.
    Category {
        /// The name of the category.
        name: String,
        /// The slot to save the matched index to.
        num: Option<usize>
    },
    /// Matches a repeating pattern
    Repeat(Box<Pattern>, Repeater),
    /// Matches a concatenation of two patterns
    Concat(Vec<Pattern>),
    /// Matches one of multiple patterns
    Alternate(Vec<Pattern>),
}

impl Pattern {
    pub fn parse_category(s: &str) -> Option<Pattern> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\{(?:([0-9]+):)?(\w+)\}$").unwrap();
        }
        RE.captures(s).map(|caps| Pattern::Category {
            name: String::from(caps.get(2).unwrap().as_str()),
            num: caps.get(1).map(|m| usize::from_str(m.as_str()).unwrap()),
        })
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Pattern::*;
        match *self {
            Empty => write!(f, ""),
            Literal(ref s) => write!(f, "{}", s),
            Any => write!(f, "."),
            WordBoundary => write!(f, "#"),
            Category{ref name, num: Some(num)} => write!(f, "{{{}:{}}}", num, name),
            Category{ref name, num: None} => write!(f, "{{{}}}", name),
            Repeat(ref pat, ref rep) => write!(f, "{}{}", pat, rep),
            Concat(ref v) =>
                write!(f, "{}", v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("")),
            Alternate(ref v) =>
                write!(f, "{}", v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("|")),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Repeater {
    ZeroOrOne(bool),
    ZeroOrMore(bool),
    OneOrMore(bool),
}

impl fmt::Display for Repeater {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Repeater::*;
        match *self {
            ZeroOrOne(greedy) => write!(f, "?{}", if greedy {""} else {"?"}),
            ZeroOrMore(greedy) => write!(f, "*{}", if greedy {""} else {"?"}),
            OneOrMore(greedy) => write!(f, "+{}", if greedy {""} else {"?"}),
        }
    }
}
