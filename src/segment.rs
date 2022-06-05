use std::convert;
use std::fmt;

use unicode_normalization::UnicodeNormalization;

use crate::{
    re::token::Token,
    unicode::{is_combining_double, is_modifier},
};

#[test]
fn test_decompose() {
    // U+E1
    let s1 = "ábc";
    // U+61 U+0301
    let s2 = "ábc";
    assert_eq!(Segment::parse_string(s1), Segment::parse_string(s2));
}

#[test]
fn test_combine() {
    let s = "t͜ʃa";
    let seg1 = Segment {
        symbol: String::from("t͜ʃ"),
        whitespace: false,
    };
    let seg2 = Segment {
        symbol: String::from("a"),
        whitespace: false,
    };
    assert_eq!(Segment::parse_string(s), vec![seg1, seg2]);
}

/// A phonological segment
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Segment {
    /// The string representation of the segment
    symbol: String,
    /// Whether or not the segment is whitespace
    whitespace: bool,
}

impl Segment {
    /// Generates a new empty segment
    fn new() -> Segment {
        Segment {
            symbol: String::from(""),
            whitespace: false,
        }
    }

    /// Parses a string to produce a vector of `Segment`s
    pub fn parse_string<S: Into<String>>(s: S) -> Vec<Segment> {
        let s = s.into();
        // this wil over-allocate if there are multi-character segments, but it's faster than
        // repeatedly growing
        let mut v = Vec::with_capacity(s.len());
        // initialize buffer to store segments
        let mut buf = Segment::new();
        // perform canonical decomposition
        let mut iter = s.chars().nfd();
        // iterate using `while let` instead of `for` so that we can skip within the iterator
        while let Some(c) = iter.next() {
            // if the buffer is empty, initialize it
            if buf.symbol.is_empty() {
                buf = Segment::from(c);
                continue;
            }
            if is_modifier(c) {
                if buf.whitespace {
                    // if buf is whitespace, push it and start a new buffer
                    v.push(buf);
                    buf = Segment::from(c);
                } else {
                    // otherwise, append to buffer
                    buf.symbol.push(c);
                    if is_combining_double(c) {
                        // if c is a combining double character, also push next char and advance
                        // iterator
                        if let Some(c) = iter.next() {
                            buf.symbol.push(c);
                        }
                    }
                }
            } else if c.is_whitespace() {
                if buf.whitespace {
                    // if buf is whitespace, append c (all whitespace is a single segment)
                    buf.symbol.push(c);
                } else {
                    // otherwise, push buf and start a new whitespace buffer
                    v.push(buf);
                    buf = Segment::from(c);
                }
            } else {
                // base character, start a new segment
                v.push(buf);
                buf = Segment::from(c);
            }
        }
        // push remaining buffer
        v.push(buf);
        // shrink vector
        v.shrink_to_fit();
        v
    }
}

impl convert::From<char> for Segment {
    /// Generates a `Segment` corresponding to the given `char`
    fn from(c: char) -> Segment {
        Segment {
            symbol: c.to_string(),
            whitespace: c.is_whitespace(),
        }
    }
}

impl Token for Segment {
    /// Returns `false` if the segment is a single space, `true` otherwise.
    fn is_word(&self) -> bool {
        !self.whitespace
    }
}

impl fmt::Display for Segment {
    /// Displays the symbol
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.symbol.fmt(f)
    }
}
