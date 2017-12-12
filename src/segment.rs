use std::convert;
use std::fmt;

use unicode_categories::UnicodeCategories;
use unicode_normalization::UnicodeNormalization;

use re::token::Token;

#[test]
fn test_decompose() {
    // U+E1
    let s1 = String::from("ábc");
    // U+61 U+0301
    let s2 = String::from("ábc");
    assert_eq!(Segment::parse_string(s1), Segment::parse_string(s2));
}

#[test]
fn test_combine() {
    let s = String::from("t͜ʃa");
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
#[derive(Debug, Eq, Hash, PartialEq)]
struct Segment {
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
    pub fn parse_string(s: String) -> Vec<Segment> {
        // this wil over-allocate if there are multi-character segments, but it's faster than
        // repeatedly growing
        let mut v = Vec::with_capacity(s.len());
        // initialize buffer to store segments
        let mut buf = Segment::new();
        // perform canonical decomposition
        let mut iter = s.chars().nfd();
        // iterate using `while let` instead of `for` so that we can skip within the iterator
        while let Some(c) = iter.next() {
            if buf.symbol == "" {
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

/// Checks if a character is a base character or a modifier. A character is considered a modifier
/// if it is in one of the following unicode classes:
/// - Lm (Letter, Modifier)
/// - Mc (Mark, Spacing Combining)
/// - Me (Mark, Enclosing)
/// - Mn (Mark, Non-Spacing)
/// - Sk (Symbol, Modifier)
/// Or, if it is a superscript or subscript
fn is_modifier(c: char) -> bool {
    // check against superscript 1, 2, and 3
    c == '\u{b9}' || c == '\u{b2}' || c == '\u{b3}'
        // check against superscripts and subscripts block
        || ('\u{2070}' <= c && c <= '\u{209f}')
        // otherwise, check if c is in the named classes
        || c.is_letter_modifier()
        || c.is_mark()
        || c.is_symbol_modifier()
}

/// Checks if a character is a modifier combining two characters
fn is_combining_double(c: char) -> bool {
    '\u{035c}' <= c && c <= '\u{0362}'
}
