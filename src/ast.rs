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

#[derive(Debug)]
pub enum Pattern {
    Empty,
    Literal(String),
    AnyChar,
    WordBoundary,
    Category {
        name: String,
        num: Option<usize>
    },
    Repeat {
        pat: Box<Pattern>,
        rep: Repeater,
        greedy: bool,
    },
    Group(Box<Pattern>),
    Concat(Vec<Pattern>),
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
            AnyChar => write!(f, "."),
            WordBoundary => write!(f, "#"),
            Category{ref name, num: Some(num)} => write!(f, "{{{}:{}}}", num, name),
            Category{ref name, num: None} => write!(f, "{{{}}}", name),
            Repeat{ref pat, ref rep, greedy: true} => write!(f, "{}{}", pat, rep),
            Repeat{ref pat, ref rep, greedy: false} => write!(f, "{}{}?", pat, rep),
            Concat(ref v) =>
                write!(f, "{}", v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("")),
            Alternate(ref v) =>
                write!(f, "{}", v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("|")),
            Group(ref p) => write!(f, "(?:{})", p),
        }
    }
}

#[derive(Debug)]
pub enum Repeater {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Range {
        min: usize,
        max: Option<usize>,
    },
}

impl Repeater {
    pub fn parse_range(s: &str) -> Option<Repeater> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\{([0-9]+)(?:,([0-9]*))?\}\??$").unwrap();
        }
        if let Some(caps) = RE.captures(s) {
            let min = usize::from_str(caps.get(1).unwrap().as_str()).unwrap();
            match caps.get(2) {
                Some(m) => Some(Repeater::Range{min, max: usize::from_str(m.as_str()).ok()}),
                None => Some(Repeater::Range{min, max: Some(min)}),
            }
        } else {
            None
        }
    }
}

impl fmt::Display for Repeater {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Repeater::*;
        match *self {
            ZeroOrOne => write!(f, "?"),
            ZeroOrMore => write!(f, "*"),
            OneOrMore => write!(f, "+"),
            Range{min, max: Some(max)} => write!(f, "{{{},{}}}", min, max),
            Range{min, max: None} => write!(f, "{{{},}}", min),
        }
    }
}
