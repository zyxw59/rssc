use std::fmt::{self, Write};

#[derive(Clone, Copy, Debug)]
pub struct EscapeArgs {
    pub escape_single_quote: bool,
    pub escape_double_quote: bool,
    pub escape_newline: bool,
    pub escape_tab: bool,
}

impl EscapeArgs {
    pub const NONE: Self = EscapeArgs {
        escape_single_quote: false,
        escape_double_quote: false,
        escape_newline: false,
        escape_tab: false,
    };

    pub const ALL: Self = EscapeArgs {
        escape_single_quote: true,
        escape_double_quote: true,
        escape_newline: true,
        escape_tab: true,
    };
}

#[derive(Debug, Clone)]
pub struct Escape(State);

#[derive(Clone, Debug)]
enum State {
    Done,
    Char(char),
    Backslash(char),
    Numeric(Numeric),
}

impl Escape {
    pub(super) fn char(ch: char) -> Self {
        Escape(State::Char(ch))
    }

    pub(super) fn backslash(ch: char) -> Self {
        Escape(State::Backslash(ch))
    }

    pub(super) fn numeric(ch: u16) -> Self {
        Escape(State::Numeric(Numeric::new(ch)))
    }

    fn len(&self) -> usize {
        match &self.0 {
            State::Done => 0,
            State::Char(_) => 1,
            State::Backslash(_) => 2,
            State::Numeric(state) => state.len(),
        }
    }
}

impl Iterator for Escape {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            State::Numeric(state) => state.next(),
            State::Backslash(ch) => {
                self.0 = State::Char(*ch);
                Some('\\')
            }
            State::Char(ch) => {
                let ch = *ch;
                self.0 = State::Done;
                Some(ch)
            }
            State::Done => None,
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.len();
        (n, Some(n))
    }

    #[inline]
    fn count(self) -> usize {
        self.len()
    }
}

impl fmt::Display for Escape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for c in self.clone() {
            f.write_char(c)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct Numeric {
    ch: u16,
    state: NumericState,

    /// The index of the next hex digit to be printed (0 if none),
    /// i.e., the number of remaining hex digits to be printed;
    /// increasing from the least significant digit: 0x3210
    hex_digit_index: usize,
}

impl Numeric {
    fn new(ch: u16) -> Self {
        let hex_digit_index = if ch <= 0xFF { 1 } else { 3 };
        Numeric {
            ch,
            state: NumericState::Backslash,
            hex_digit_index,
        }
    }

    fn len(&self) -> usize {
        self.hex_digit_index
            + match self.state {
                NumericState::Done => 0,
                NumericState::Digit => 1,
                NumericState::Type => 2,
                NumericState::Backslash => 3,
            }
    }
}

#[derive(Clone, Debug)]
enum NumericState {
    Done,
    Digit,
    Type,
    Backslash,
}

impl Iterator for Numeric {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            NumericState::Backslash => {
                self.state = NumericState::Type;
                Some('\\')
            }
            NumericState::Type => {
                self.state = NumericState::Digit;
                if self.ch <= 0xFF {
                    Some('x')
                } else {
                    Some('u')
                }
            }
            NumericState::Digit => {
                let hex_digit = (self.ch >> (self.hex_digit_index * 4)) & 0xF;
                let c = std::char::from_digit(hex_digit as u32, 16).unwrap();
                if self.hex_digit_index == 0 {
                    self.state = NumericState::Done;
                } else {
                    self.hex_digit_index -= 1;
                }
                Some(c)
            }
            NumericState::Done => None,
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.len();
        (n, Some(n))
    }

    #[inline]
    fn count(self) -> usize {
        self.len()
    }
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for c in self.clone() {
            f.write_char(c)?;
        }
        Ok(())
    }
}
