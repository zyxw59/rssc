//! A limited version of regex for tokenizing and parsing input.

use std::borrow::Borrow;
use std::fmt;
use std::mem;
use std::ops::Index;

/// Type for indexing into a program
pub type InstrPtr = usize;

/// A trait for extensions to the Regex engine.
///
/// An extension provides instructions to the Regex engine for matching characters, while the base
/// `Instr` type provides the control-flow instructions.
pub trait RegexExtension {
    /// The type of token matched.
    type Token: Copy;

    /// Determines whether the given character matches the instruction.
    fn is_match(&self, tok: Self::Token) -> bool;
}

/// A single instruction
#[derive(Debug)]
pub enum Instr<R> {
    /// Matches a token using the `RegexExtension`.
    Token(R),
    /// Splits into two states, preferring not to jump.
    Split(InstrPtr),
    /// Jumps to a new point in the program.
    Jump(InstrPtr),
    /// Pushes the current index to the save list.
    Save,
    /// The end of a match.
    Match,
}

impl<R: fmt::Display> fmt::Display for Instr<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instr::Token(r) => write!(f, "Token({r})"),
            Instr::Split(x) => write!(f, "Split({x:02X})"),
            Instr::Jump(x) => write!(f, "Jump({x:02X})"),
            Instr::Save => f.write_str("Save"),
            Instr::Match => f.write_str("Match"),
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
    fn add_thread<R>(
        &mut self,
        pc: InstrPtr,
        in_idx: usize,
        prog: &Program<R>,
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
        match prog[pc] {
            Instr::Split(split) => {
                // call `add_thread` recursively
                // branch with no jump is higher priority
                // clone the `saved` vector so we can use it again in the second branch
                self.add_thread(pc + 1, in_idx, prog, saved.clone(), last);
                self.add_thread(split, in_idx, prog, saved, last);
            }
            Instr::Jump(jump) => {
                // call `add_thread` recursively
                // jump to specified pc
                self.add_thread(jump, in_idx, prog, saved, last);
            }
            Instr::Save => {
                // save index
                saved.push(in_idx);
                // and recursively add next instruction
                self.add_thread(pc + 1, in_idx, prog, saved, last);
            }
            Instr::Token(_) | Instr::Match => {
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
pub struct Program<R> {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    prog: Vec<Instr<R>>,
    /// First instruction to execute.
    start: InstrPtr,
}

impl<R: RegexExtension> Program<R> {
    pub fn new(prog: Vec<Instr<R>>, start: InstrPtr) -> Program<R> {
        Program { prog, start }
    }

    /// Executes the program. Returns the positions of all the save locations if a match is found.
    pub fn exec<C, I>(&self, input: I) -> Option<SaveList>
    where
        C: Borrow<R::Token>,
        I: IntoIterator<Item = C>,
    {
        // initialize thread lists. The number of threads should be limited by the length of the
        // program (since each instruction either ends a thread (in the case of a `Match` or a
        // failed `Token` instruction), continues an existing thread (in the case of a successful
        // `Token`, `Jump`, or `Save` instruction), or spawns a new thread (in the case of a `Split`
        // instruction))
        let mut curr = ThreadList::new(self.prog.len());
        let mut next = ThreadList::new(self.prog.len());
        let mut last = vec![OptionUsize::none(); self.prog.len()];

        let mut saves = Vec::new();

        // start initial thread at start instruction
        curr.add_thread(0, 0, self, SaveList::new(), &mut last);

        // iterate over tokens of input string
        for (i, ch_i) in input.into_iter().enumerate() {
            let ch_i = *ch_i.borrow();
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            for th in &mut curr {
                match &self[th.pc] {
                    Instr::Token(matcher) => {
                        // check if char matches
                        if matcher.is_match(ch_i) {
                            // increment thread pc, passing along next input index, and saved
                            // positions
                            next.add_thread(th.pc + 1, i + 1, self, th.saved, &mut last);
                        }
                    }
                    Instr::Match => {
                        // add the saved locations to the final list
                        saves.push(th.saved);
                    }
                    // These instructions are handled in add_thread, so the current thread should
                    // never point to one of them
                    Instr::Split(_) | Instr::Jump(_) | Instr::Save => {
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
            if let Instr::Match = self[th.pc] {
                saves.push(th.saved);
            }
        }

        // return the list of saved locations
        saves.into_iter().next()
    }
}

impl<R> Index<InstrPtr> for Program<R> {
    type Output = Instr<R>;

    fn index(&self, idx: InstrPtr) -> &Instr<R> {
        self.prog.index(idx)
    }
}

impl<R: fmt::Display> fmt::Display for Program<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.prog.iter().enumerate() {
            writeln!(f, "{i:02X}: {instr}")?;
        }
        Ok(())
    }
}
