use std::collections::{HashMap, HashSet};
use std::mem;
use std::ops::Index;

use re::token::Token;

/// Type for indexing into a program
pub type InstrPtr = usize;

/// A single instruction
#[derive(Debug)]
pub enum Instr<T: Token> {
    /// Matches a single token.
    Token(T),
    /// Matches any token.
    Any,
    /// Maps input tokens to new `InstrPtr`s. If the input token is not found, falls through to the
    /// next instruction.
    Map(HashMap<T, InstrPtr>),
    /// Matches a single token from a set of tokens.
    Set(HashSet<T>),
    /// Matches a word boundary.
    WordBoundary,
    /// Splits into two states, preferring not to jump. Used to implement alternations and
    /// quantifiers
    Split(InstrPtr),
    /// Splits into two states, preferring to jump. Used to implement alternations and quantifiers.
    JSplit(InstrPtr),
    /// Jumps to a new point in the program.
    Jump(InstrPtr),
    /// Saves the current index to the indicated save slot. Used for subgroup matching. In general,
    /// save the start of the <i>n</i>th capturing group to slot _2n_, and the end to slot _2n +
    /// 1_.
    Save(usize),
    /// Reject a potential match. Can be used after a Map when fallthrough should fail.
    Reject,
    /// The end of a match.
    Match,
}

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
    /// locations. If `pc` points to a `Jump`, `Split`, `JSplit`, or `Save` instruction, calls
    /// `add_thread` recursively, so that the active `ThreadList` never contains pointers to those
    /// instructions.
    fn add_thread<T: Token>(
        &mut self,
        pc: InstrPtr,
        in_idx: usize,
        prog: &Program<T>,
        mut saved: SaveList,
    ) {
        // don't check if there's already a thread with this `pc` on the list, because we want to
        // keep alternate paths alive, in case they produce different submatch values.
        use self::Instr::*;
        match prog[pc] {
            Split(split) => {
                // call `add_thread` recursively
                // branch with no jump is higher priority
                // clone the `saved` vector so we can use it again in the second branch
                self.add_thread(pc + 1, in_idx, prog, saved.clone());
                self.add_thread(split, in_idx, prog, saved);
            }
            JSplit(split) => {
                // call `add_thread` recursively
                // branch with jump is higher priority
                // clone the `saved` vector so we can use it again in the second branch
                self.add_thread(split, in_idx, prog, saved.clone());
                self.add_thread(pc + 1, in_idx, prog, saved);
            }
            Jump(jump) => {
                // call `add_thread` recursively
                // jump to specified pc
                self.add_thread(jump, in_idx, prog, saved);
            }
            Save(idx) => {
                // save index
                saved[idx] = Some(in_idx);
                // and recursively add next instruction
                self.add_thread(pc + 1, in_idx, prog, saved);
            }
            Reject => {} // do nothing, this thread is dead
            Token(_) | Map(_) | Set(_) | Any | WordBoundary | Match => {
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

/// A list of saved locations, which may be absent
pub type SaveList = Vec<Option<usize>>;

/// A program for the VM
pub struct Program<T: Token> {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    prog: Vec<Instr<T>>,
    /// Number of save slots. Generally _2n + 2_, where _n_ is the number of capturing groups,
    /// since the first two slots are used for the entire match.
    num_slots: usize,
}

impl<T: Token> Program<T> {
    pub fn new(prog: Vec<Instr<T>>, num_slots: usize) -> Program<T> {
        Program { prog, num_slots }
    }

    /// Executes the program. Returns a vector of matches found. For each match, the positions of
    /// all the save locations are stored in a vector
    pub fn exec<I>(&self, input: I) -> Vec<SaveList>
    where
        I: IntoIterator<Item = T>,
    {
        // initialize thread lists. The number of threads should be limited by the length of the
        // program (since each instruction either ends a thread (in the case of a `Match` or a
        // failed `Token` instruction), continues an existing thread (in the case of a successful
        // `Token`, `Jump`, or `Save` instruction), or spawns a new thread (in the case of a
        // `Split` or `JSplit` instruction))
        let mut curr = ThreadList::new(self.prog.len());
        let mut next = ThreadList::new(self.prog.len());

        let mut saves = Vec::new();

        // start initial thread at start instruction
        curr.add_thread(0, 0, self, vec![None; self.num_slots]);

        // set initial word flag
        let mut word = false;

        // to store the iteration number (declaring it here so it can be used in the final checks
        // after the loop)
        let mut i = 0;

        // iterate over tokens of input string
        for tok_i in input {
            // check if word boundary
            let new_word = tok_i.is_word();
            let word_boundary = new_word ^ word;
            word = new_word;
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            for th in &mut curr {
                use self::Instr::*;
                match self[th.pc] {
                    Token(ref token) => {
                        // check if token matches
                        if &tok_i == token {
                            // increment thread pc, passing along next input index, and saved
                            // positions
                            next.add_thread(th.pc + 1, i + 1, self, th.saved);
                        }
                    }
                    Set(ref set) => {
                        // check if token in set
                        if set.contains(&tok_i) {
                            // increment thread pc, passing along next input index, and saved
                            // positions
                            next.add_thread(th.pc + 1, i + 1, self, th.saved);
                        }
                    }
                    Map(ref map) => {
                        // get the corresponding pc, or default to incrementing
                        next.add_thread(
                            map.get(&tok_i).cloned().unwrap_or(th.pc + 1),
                            i + 1,
                            self,
                            th.saved,
                        );
                    }
                    Any => {
                        // always matches
                        next.add_thread(th.pc + 1, i + 1, self, th.saved);
                    }
                    WordBoundary => {
                        // check if word boundary
                        if word_boundary {
                            next.add_thread(th.pc + 1, i, self, th.saved);
                        }
                    }
                    Match => {
                        // add the saved locations to the final list
                        saves.push(th.saved);
                    }
                    // These instructions are handled in add_thread, so the current thread should
                    // never point to one of them
                    Split(_) | JSplit(_) | Jump(_) | Save(_) | Reject => {
                        unreachable!();
                    }
                }
            }
            // `next` becomes list of active threads, and `curr` (empty after iteration) can hold
            // the next iteration
            mem::swap(&mut curr, &mut next);
            // increment`i`
            i += 1;
        }

        // now iterate over remaining threads, to check for pending word boundary instructions
        for th in &mut curr {
            use self::Instr::*;
            match self[th.pc] {
                WordBoundary => {
                    // check if last token was a word token
                    if word {
                        next.add_thread(th.pc + 1, i, self, th.saved);
                    }
                }
                Match => {
                    saves.push(th.saved);
                }
                // anything else is a failed match
                _ => {}
            }
        }

        // now iterate over remaining threads, to check for pending match instructions
        for th in &mut next {
            if let Instr::Match = self[th.pc] {
                saves.push(th.saved);
            }
        }

        // return the list of saved locations
        saves
    }
}

impl<T: Token> Index<InstrPtr> for Program<T> {
    type Output = Instr<T>;

    fn index(&self, idx: InstrPtr) -> &Instr<T> {
        self.prog.index(idx)
    }
}
