use std::convert;
use std::fmt;

use unicode_categories::UnicodeCategories;
use unicode_normalization::UnicodeNormalization;

use re::token::Token;

/// A phonological segment
#[derive(Debug,Eq,Hash)]
struct Segment {
    symbol: String,
    whitespace: bool;
}

impl Segment {
    fn new() -> Segment {
        Segment {
            symbol: String::from(""),
            whitespace: false,
        }
    }

    /// Parses a string to produce a vector of `Segments`
    fn parse_string(s: String) -> Vec<Segment> {
        // this wil over-allocate if there are multi-character segments, but it's faster than
        // repeatedly growing
        let mut v = Vec::with_capacity(s.len());
        let mut buf = Segment::new();
        // perform canonical decomposition
        let mut iter = s.chars().nfd();
        // iterate using `while let` instead of `for` so that we can skip within the iterator
        while let Some(c) = iter.next() {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.symbol.fmt(f)
    }
}

/// Checks if a character is a base character or a modifier. A character is considered a modifier
/// if it is in one of the following unicode classes:
/// - Lm (Letter, Modifier)
/// - Mn (Mark, Non-Spacing)
/// - Sk (Symbol, Modifier)
/// Or, if it is a superscript or subscript 
fn is_modifier(c: char) -> bool {
    // check against superscript 1, 2, and 3
    if c == 0xb9 || c == 0xb2 || c == 0xb3 {
        return true;
    }
    // check against superscripts and subscripts block
    if 0x2070 <= c && c <= 0x209f {
        return true;
    }
    // otherwise, check if c is in the named classes
    c.is_letter_modifier() || c.is_mark_nonspacing() || c.is_symbol_modifier()
}

/// Checks if a character is a modifier combining two characters
fn is_combining_double(c: char) -> bool {
    0x035c <= c && c <= 0x0362
}
