use std::fmt;

use unicode_categories::UnicodeCategories;

use super::segment::SegmentMap;
use super::re::{Instr, Program, RegexExtension};

#[cfg(test)]
mod tests {
    use super::*;
    use unicode_normalization::UnicodeNormalization;

    #[test]
    fn multiple_possible_segmentations() {
        let line = String::from("antś\n");
        let line = line.chars().nfd();
        let segments =
            SegmentMap::clone_from_vec(&vec![vec!['a', 'n'], vec!['n', 't'], vec!['t', 's']]);
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2, 3, 5, 6]));
    }

    #[test]
    fn combining_double() {
        let line = String::from("t͜s\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 3, 4]));
    }

    #[test]
    fn backslash() {
        let line = String::from("\\.\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2, 3]));
    }

    #[test]
    fn backslash_newline() {
        let line = String::from("\\\n");
        let line = line.chars().nfd();
        let segments = SegmentMap::clone_from_vec(&Vec::new());
        let prog = matcher(&segments);
        println!("{}", prog);

        let saves = prog.exec(line);
        assert_eq!(saves, Some(vec![0, 2]));
    }
}

/// A `RegexExtension` for the tokenizer.
#[derive(Clone, Copy, Debug)]
pub enum TokenizerExtension {
    /// Matches a single char.
    Char(char),
    /// Matches any character.
    Any,
    /// Matches a control character.
    ControlChar,
    /// Matches a non-combining character.
    BaseChar,
    /// Matches a combining character.
    CombiningChar,
    /// Matches a combining double character.
    CombiningDouble,
}

impl RegexExtension for TokenizerExtension {
    type Token = char;

    fn is_match(&self, tok: char) -> bool {
        use self::TokenizerExtension::*;
        match *self {
            Char(ch) => tok == ch,
            Any => true,
            ControlChar => super::Token::is_control_char(tok),
            BaseChar => !is_modifier(tok),
            CombiningChar => is_modifier(tok) && !is_combining_double(tok),
            CombiningDouble => is_combining_double(tok),
        }
    }
}

impl fmt::Display for TokenizerExtension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
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
        || c.is_letter_modifier() || c.is_mark() || c.is_symbol_modifier()
}

/// Checks if a character is a modifier combining two characters
fn is_combining_double(c: char) -> bool {
    '\u{035c}' <= c && c <= '\u{0362}'
}

/// Construct a regex program to split a line into segments.
pub fn matcher(segments: &SegmentMap) -> Program<TokenizerExtension> {
    // the instructions
    let mut prog = Vec::new();

    use self::TokenizerExtension::*;

    // save instruction, to be performed at the end of each token
    prog.push(Instr::Save);
    // match a control character
    let mut split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(ControlChar));
    prog.push(Instr::Jump(0));
    // match a backslash-escaped character, which is considered as a single base character
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(Char('\\')));
    prog.push(Instr::Split(split + 5));
    prog.push(Instr::Token(Char('\n')));
    prog.push(Instr::Jump(0));
    prog.push(Instr::Token(Any));
    let escape_jump = prog.len();
    prog.push(Instr::Jump(0));
    // match a newline, and therefore the end of the string
    prog[split] = Instr::Split(prog.len());
    split = prog.len();
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(Char('\n')));
    prog[escape_jump - 2] = Instr::Jump(prog.len());
    prog.push(Instr::Save);
    prog.push(Instr::Match);
    // match user-defined segments
    for seg in segments.iter() {
        prog[split] = Instr::Split(prog.len());
        split = prog.len();
        prog.push(Instr::Split(0));
        for c in seg {
            prog.push(Instr::Token(Char(*c)));
        }
        prog.push(Instr::Jump(0));
    }
    // match a base character followed by any number of combining characters
    let base = prog.len();
    prog[escape_jump] = Instr::Jump(base + 1);
    prog[split] = Instr::Split(base);
    prog.push(Instr::Token(BaseChar));
    prog.push(Instr::Split(base + 4));
    prog.push(Instr::Token(CombiningChar));
    prog.push(Instr::Jump(base + 1));
    prog.push(Instr::Split(0));
    prog.push(Instr::Token(CombiningDouble));
    prog.push(Instr::Jump(base));

    Program::new(prog, 0)
}
