//! A limited version of regex for performing tokenization of input

use std::borrow::Borrow;
use std::fmt;
use std::mem;
use std::ops::Index;

use unicode_categories::UnicodeCategories;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program() {
        use self::Instr::*;
        let prog = vec![
            // 0
            Split(3),
            // 1
            ControlChar,
            // 2
            Jump(28),
            // 3
            Split(6),
            // 4
            Char('\\'),
            // 5
            Jump(21),
            // 6
            Split(10),
            // 7
            Char('a'),
            // 8
            Char('n'),
            // 9
            Jump(28),
            // 10
            Split(14),
            // 11
            Char('n'),
            // 12
            Char('t'),
            // 13
            Jump(28),
            // 14
            Split(18),
            // 15
            Char('t'),
            // 16
            Char('s'),
            // 17
            Jump(28),
            // 18
            Split(21),
            // 19
            Char('\n'),
            // 20
            Match,
            // 21
            BaseChar,
            // 22
            Split(25),
            // 23
            CombiningChar,
            // 24
            Jump(22),
            // 25
            Split(28),
            // 26
            CombiningDouble,
            // 27
            Jump(21),
            // 28
            Save,
            // 29
            Jump(0),
        ];
        let program = Program::new(prog, 0);
        let saves = program.exec("antś\n".chars());
        assert_eq!(saves, Some(vec![2, 3, 5]));
    }

    #[test]
    fn combining_double() {
        use self::Instr::*;
        let program = Program::new(vec![BaseChar, CombiningDouble, BaseChar, Match], 0);
        let saves = program.exec("t͜s".chars());
        println!("{:?}", "t͜s".chars().collect::<Vec<_>>());
        assert!(saves.is_some());
    }
}

/// Type for indexing into a program
pub type InstrPtr = usize;

/// A single instruction
#[derive(Debug)]
pub enum Instr {
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
    /// Splits into two states, preferring not to jump.
    Split(InstrPtr),
    /// Jumps to a new point in the program.
    Jump(InstrPtr),
    /// Pushes the current index to the save list.
    Save,
    /// The end of a match.
    Match,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instr::Split(x) => write!(f, "Split({:02X})", x),
            Instr::Jump(x) => write!(f, "Jump({:02X})", x),
            _ => write!(f, "{:?}", self),
        }
    }
}

/// A list of saved positions in the input string
type SaveList = Vec<usize>;

/// A thread, consisting of an `InstrPtr` to the current instruction, and a vector of all saved
/// positions
#[derive(Debug)]
struct Thread {
    /// Pointer to current instruction
    pc: InstrPtr,
    /// Saved positions
    saved: SaveList,
}

impl Thread {
    /// Create a new `Thread` with the specified instruction pointer and the given list of saved
    /// locations.
    fn new(pc: InstrPtr, saved: SaveList) -> Thread {
        Thread { pc, saved }
    }
}

/// An optimization of `Option<usize>` where we know the MSB will never be set because we will OOM
/// first.
#[derive(Clone, Copy, Eq, PartialEq)]
struct OptionUsize(usize);

/// The MSB of a `usize`
const MSB: usize = !(::std::usize::MAX >> 1);

impl OptionUsize {
    pub fn some(x: usize) -> OptionUsize {
        OptionUsize(x | MSB)
    }

    pub fn none() -> OptionUsize {
        OptionUsize(0)
    }

    pub fn as_option(&self) -> Option<usize> {
        if self.0 & MSB == 0 {
            None
        } else {
            Some(self.0 & !MSB)
        }
    }
}

impl fmt::Debug for OptionUsize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_option())
    }
}

/// A list of threads
#[derive(Debug)]
struct ThreadList {
    threads: Vec<Thread>,
}

impl ThreadList {
    /// Create a new `ThreadList` with a specified capacity
    fn new(cap: usize) -> ThreadList {
        ThreadList {
            threads: Vec::with_capacity(cap),
        }
    }

    /// Add a new `Thread` with the specified instruction pointer, and the given list of saved
    /// locations. If `pc` points to a `Jump`, `Split`, or `Save` instruction, calls `add_thread`
    /// recursively, so that the active `ThreadList` never contains pointers to those instructions.
    fn add_thread(
        &mut self,
        pc: InstrPtr,
        in_idx: usize,
        prog: &Program,
        mut saved: SaveList,
        last: &mut Vec<OptionUsize>,
    ) {
        // check if this `pc` has already been used at this `in_idx`
        if let Some(i) = last[pc].as_option() {
            if i == in_idx {
                // we've already encountered this `pc` at this `in_idx`
                return;
            }
        }
        last[pc] = OptionUsize::some(in_idx);
        use self::Instr::*;
        match prog[pc] {
            Split(split) => {
                // call `add_thread` recursively
                // branch with no jump is higher priority
                // clone the `saved` vector so we can use it again in the second branch
                self.add_thread(pc + 1, in_idx, prog, saved.clone(), last);
                self.add_thread(split, in_idx, prog, saved, last);
            }
            Jump(jump) => {
                // call `add_thread` recursively
                // jump to specified pc
                self.add_thread(jump, in_idx, prog, saved, last);
            }
            Save => {
                // save index
                saved.push(in_idx);
                // and recursively add next instruction
                self.add_thread(pc + 1, in_idx, prog, saved, last);
            }
            Char(_) | Any | ControlChar | BaseChar | CombiningChar | CombiningDouble | Match => {
                // push a new thread with the given pc
                self.threads.push(Thread::new(pc, saved));
            }
        }
    }
}

impl<'a> IntoIterator for &'a mut ThreadList {
    type Item = Thread;
    type IntoIter = ::std::vec::Drain<'a, Thread>;

    fn into_iter(self) -> Self::IntoIter {
        self.threads.drain(..)
    }
}

/// A program for the VM
#[derive(Debug)]
pub struct Program {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    prog: Vec<Instr>,
    /// First instruction to execute.
    start: InstrPtr,
}

impl Program {
    pub fn new(prog: Vec<Instr>, start: InstrPtr) -> Program {
        Program { prog, start }
    }

    /// Executes the program. Returns the positions of all the save locations if a match is found.
    pub fn exec<C, I>(&self, input: I) -> Option<SaveList>
    where
        C: Borrow<char>,
        I: IntoIterator<Item = C>,
    {
        // initialize thread lists. The number of threads should be limited by the length of the
        // program (since each instruction either ends a thread (in the case of a `Match` or a
        // failed `Char` instruction), continues an existing thread (in the case of a successful
        // `Char`, `Jump`, or `Save` instruction), or spawns a new thread (in the case of a `Split`
        // instruction))
        let mut curr = ThreadList::new(self.prog.len());
        let mut next = ThreadList::new(self.prog.len());
        let mut last = vec![OptionUsize::none(); self.prog.len()];

        let mut saves = Vec::new();

        // start initial thread at start instruction
        curr.add_thread(0, 0, self, SaveList::new(), &mut last);

        // iterate over tokens of input string
        for (i, ref ch_i) in input.into_iter().enumerate() {
            let ch_i = *ch_i.borrow();
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            for th in &mut curr {
                use self::Instr::*;
                match self[th.pc] {
                    Char(ref ch_match) => {
                        // check if char matches
                        if ch_i == *ch_match {
                            // increment thread pc, passing along next input index, and saved
                            // positions
                            next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                        }
                    }
                    Any => {
                        // always matches
                        next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                    }
                    ControlChar => {
                        // check if the character is a control character
                        if super::Token::is_control_char(ch_i) {
                            next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                        }
                    }
                    BaseChar => {
                        // check if the character is a base character
                        if !is_modifier(ch_i) {
                            next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                        }
                    }
                    CombiningChar => {
                        // check if the character is a combining character
                        if is_modifier(ch_i) {
                            next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                        }
                    }
                    CombiningDouble => {
                        // check if the character is a combining double character
                        if is_combining_double(ch_i) {
                            next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                        }
                    }
                    Match => {
                        // add the saved locations to the final list
                        saves.push(th.saved);
                    }
                    // These instructions are handled in add_thread, so the current thread should
                    // never point to one of them
                    Split(_) | Jump(_) | Save => {
                        unreachable!();
                    }
                }
            }
            // `next` becomes list of active threads, and `curr` (empty after iteration) can hold
            // the next iteration
            mem::swap(&mut curr, &mut next);
        }

        // now iterate over remaining threads, to check for pending match instructions
        for th in &mut curr {
            use self::Instr::*;
            match self[th.pc] {
                Match => {
                    saves.push(th.saved);
                }
                // anything else is a failed match
                _ => {}
            }
        }

        // return the list of saved locations
        saves.into_iter().next()
    }
}

impl Index<InstrPtr> for Program {
    type Output = Instr;

    fn index(&self, idx: InstrPtr) -> &Instr {
        self.prog.index(idx)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.prog.iter().enumerate() {
            writeln!(f, "{:02X}: {}", i, instr)?;
        }
        Ok(())
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
    (c == '\u{b9}' || c == '\u{b2}' || c == '\u{b3}'
     // check against superscripts and subscripts block
     || ('\u{2070}' <= c && c <= '\u{209f}')
     // otherwise, check if c is in the named classes
     || c.is_letter_modifier()
     || c.is_mark()
     || c.is_symbol_modifier())
        // but don't allow combining double characters
        && !is_combining_double(c)
}

/// Checks if a character is a modifier combining two characters
fn is_combining_double(c: char) -> bool {
    '\u{035c}' <= c && c <= '\u{0362}'
}
