use std::collections::{HashMap, HashSet};
use std::mem;
use std::ops::Index;

use super::engine::Engine;
use super::token::Token;

/// Type for indexing into a program
pub type InstrPtr = usize;

/// A single instruction
#[derive(Debug)]
pub enum Instr<T: Token, E: Engine<T>> {
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
    /// Consumes a token. The engine determines whether it matches.
    Consume(E::Consume),
    /// Peeks at the next token without consuming it. The engine determines whether it matches.
    Peek(E::Peek),
    /// Reject a potential match. Can be used after a Map when fallthrough should fail.
    Reject,
    /// The end of a match.
    Match,
}

/// A thread, consisting of an `InstrPtr` to the current instruction, and a vector of all saved
/// positions
#[derive(Debug)]
struct Thread<E> {
    /// Pointer to current instruction
    pc: InstrPtr,
    /// Implementation-specific state
    engine: E,
}

impl<E> Thread<E> {
    /// Create a new `Thread` with the specified instruction pointer and the given state.
    fn new(pc: InstrPtr, engine: E) -> Self {
        Thread { pc, engine }
    }
}

/// A list of threads
#[derive(Debug)]
struct ThreadList<E> {
    threads: Vec<Thread<E>>,
}

impl<E> ThreadList<E> {
    /// Create a new `ThreadList` with a specified capacity
    fn new(cap: usize) -> Self {
        ThreadList {
            threads: Vec::with_capacity(cap),
        }
    }

    /// Add a new `Thread` with the specified instruction pointer, and the given list of saved
    /// locations. If `pc` points to a `Jump`, `Split`, `JSplit`, or `Peek` instruction, calls
    /// `add_thread` recursively, so that the active `ThreadList` never contains pointers to those
    /// instructions.
    fn add_thread<T: Token>(
        &mut self,
        pc: InstrPtr,
        in_idx: usize,
        next_tok: Option<&T>,
        prog: &Program<T, E>,
        mut engine: E,
    ) where
        E: Engine<T>,
    {
        // don't check if there's already a thread with this `pc` on the list, because we want to
        // keep alternate paths alive, in case they produce different submatch values.
        match prog[pc] {
            Instr::Split(split) => {
                // call `add_thread` recursively
                // branch with no jump is higher priority
                // clone the `engine` so we can use it again in the second branch
                self.add_thread(pc + 1, in_idx, next_tok, prog, engine.clone());
                self.add_thread(split, in_idx, next_tok, prog, engine);
            }
            Instr::JSplit(split) => {
                // call `add_thread` recursively
                // branch with jump is higher priority
                // clone the `engine` so we can use it again in the second branch
                self.add_thread(split, in_idx, next_tok, prog, engine.clone());
                self.add_thread(pc + 1, in_idx, next_tok, prog, engine);
            }
            Instr::Jump(jump) => {
                // call `add_thread` recursively
                // jump to specified pc
                self.add_thread(jump, in_idx, next_tok, prog, engine);
            }
            Instr::Peek(ref args) => {
                // check if the engine matches here
                if engine.peek(args, in_idx, next_tok) {
                    // and recursively add next instruction
                    self.add_thread(pc + 1, in_idx, next_tok, prog, engine);
                }
            }
            Instr::Reject => {} // do nothing, this thread is dead
            Instr::Token(_)
            | Instr::Map(_)
            | Instr::Set(_)
            | Instr::Any
            | Instr::WordBoundary
            | Instr::Consume(_)
            | Instr::Match => {
                // push a new thread with the given pc
                self.threads.push(Thread::new(pc, engine));
            }
        }
    }
}

impl<'a, E> IntoIterator for &'a mut ThreadList<E> {
    type Item = Thread<E>;
    type IntoIter = ::std::vec::Drain<'a, Thread<E>>;

    fn into_iter(self) -> Self::IntoIter {
        self.threads.drain(..)
    }
}

/// A program for the VM
pub struct Program<T: Token, E: Engine<T>> {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    prog: Vec<Instr<T, E>>,
    /// Initialization arguments for the engine
    init: E::Init,
}

impl<T: Token, E: Engine<T>> Program<T, E> {
    pub fn new(prog: Vec<Instr<T, E>>, init: E::Init) -> Program<T, E> {
        Program { prog, init }
    }

    /// Executes the program. Returns a vector of matches found. For each match, the positions of
    /// all the save locations are stored in a vector
    pub fn exec<I>(&self, input: I) -> Vec<E>
    where
        I: IntoIterator<Item = T>,
    {
        let mut input = input.into_iter().peekable();

        // initialize thread lists. The number of threads should be limited by the length of the
        // program (since each instruction either ends a thread (in the case of a `Match` or a
        // failed `Token` instruction), continues an existing thread (in the case of a successful
        // `Token`, `Jump`, or `Peek` instruction), or spawns a new thread (in the case of a
        // `Split` or `JSplit` instruction))
        let mut curr = ThreadList::new(self.prog.len());
        let mut next = ThreadList::new(self.prog.len());

        let mut saves = Vec::new();

        // start initial thread at start instruction
        curr.add_thread(0, 0, input.peek(), self, E::initialize(&self.init));

        // set initial word flag
        let mut word = false;

        // to store the iteration number (declaring it here so it can be used in the final checks
        // after the loop)
        let mut i = 0;

        // iterate over tokens of input string
        while let Some(tok_i) = input.next() {
            // check if word boundary
            let new_word = tok_i.is_word();
            let word_boundary = new_word ^ word;
            word = new_word;
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            for mut th in &mut curr {
                match &self[th.pc] {
                    Instr::Token(token) => {
                        // check if token matches
                        if &tok_i == token {
                            // increment thread pc, passing along next input index, and saved
                            // positions
                            next.add_thread(th.pc + 1, i + 1, input.peek(), self, th.engine);
                        }
                    }
                    Instr::Set(set) => {
                        // check if token in set
                        if set.contains(&tok_i) {
                            // increment thread pc, passing along next input index, and saved
                            // positions
                            next.add_thread(th.pc + 1, i + 1, input.peek(), self, th.engine);
                        }
                    }
                    Instr::Map(map) => {
                        // get the corresponding pc, or default to incrementing
                        next.add_thread(
                            map.get(&tok_i).cloned().unwrap_or(th.pc + 1),
                            i + 1,
                            input.peek(),
                            self,
                            th.engine,
                        );
                    }
                    Instr::Any => {
                        // always matches
                        next.add_thread(th.pc + 1, i + 1, input.peek(), self, th.engine);
                    }
                    Instr::WordBoundary => {
                        // check if word boundary
                        if word_boundary {
                            next.add_thread(th.pc + 1, i, input.peek(), self, th.engine);
                        }
                    }
                    Instr::Consume(args) => {
                        if th.engine.consume(args, i, &tok_i) {
                            next.add_thread(th.pc + 1, i + 1, input.peek(), self, th.engine);
                        }
                    }
                    Instr::Match => {
                        // add the saved locations to the final list
                        saves.push(th.engine);
                    }
                    // These instructions are handled in add_thread, so the current thread should
                    // never point to one of them
                    Instr::Split(_)
                    | Instr::JSplit(_)
                    | Instr::Jump(_)
                    | Instr::Peek(_)
                    | Instr::Reject => {
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
            match self[th.pc] {
                Instr::WordBoundary => {
                    // check if last token was a word token
                    if word {
                        next.add_thread(th.pc + 1, i, None, self, th.engine);
                    }
                }
                Instr::Match => {
                    saves.push(th.engine);
                }
                // anything else is a failed match
                _ => {}
            }
        }

        // now iterate over remaining threads, to check for pending match instructions
        for th in &mut next {
            if let Instr::Match = self[th.pc] {
                saves.push(th.engine);
            }
        }

        // return the list of saved locations
        saves
    }
}

impl<T: Token, E: Engine<T>> Index<InstrPtr> for Program<T, E> {
    type Output = Instr<T, E>;

    fn index(&self, idx: InstrPtr) -> &Instr<T, E> {
        self.prog.index(idx)
    }
}
