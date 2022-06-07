use std::fmt;
use std::mem;
use std::ops::Index;

use super::{engine::Engine, prune::PruneList};

/// Type for indexing into a program
pub type InstrPtr = usize;

/// A single instruction
#[derive(derivative::Derivative)]
#[derivative(Debug(bound = "E::Consume: fmt::Debug, E::Peek: fmt::Debug"))]
pub enum Instr<T, E: Engine<T>> {
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
    fn add_thread<T>(
        &mut self,
        pc: InstrPtr,
        in_idx: usize,
        next_tok: Option<&T>,
        prog: &Program<T, E>,
        prune_list: &mut PruneList,
        mut engine: E,
    ) where
        E: Engine<T>,
    {
        // prune this thread if necessary
        if prune_list.insert(pc, &engine, in_idx) {
            return;
        }

        match prog[pc] {
            Instr::Split(split) => {
                // call `add_thread` recursively
                // branch with no jump is higher priority
                // clone the `engine` so we can use it again in the second branch
                self.add_thread(pc + 1, in_idx, next_tok, prog, prune_list, engine.clone());
                self.add_thread(split, in_idx, next_tok, prog, prune_list, engine);
            }
            Instr::JSplit(split) => {
                // call `add_thread` recursively
                // branch with jump is higher priority
                // clone the `engine` so we can use it again in the second branch
                self.add_thread(split, in_idx, next_tok, prog, prune_list, engine.clone());
                self.add_thread(pc + 1, in_idx, next_tok, prog, prune_list, engine);
            }
            Instr::Jump(jump) => {
                // call `add_thread` recursively
                // jump to specified pc
                self.add_thread(jump, in_idx, next_tok, prog, prune_list, engine);
            }
            Instr::Peek(ref args) => {
                // check if the engine matches here
                if engine.peek(args, in_idx, next_tok) {
                    // and recursively add next instruction
                    self.add_thread(pc + 1, in_idx, next_tok, prog, prune_list, engine);
                }
            }
            Instr::Reject => {} // do nothing, this thread is dead
            Instr::Consume(_) | Instr::Match => {
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
#[derive(derivative::Derivative)]
#[derivative(Debug(bound = "E::Init: fmt::Debug, E::Consume: fmt::Debug, E::Peek: fmt::Debug"))]
pub struct Program<T, E: Engine<T>> {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    prog: Vec<Instr<T, E>>,
    /// Initialization arguments for the engine
    init: E::Init,
}

impl<T, E: Engine<T>> fmt::Display for Program<T, E>
where
    E::Peek: fmt::Debug,
    E::Consume: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.prog.iter().enumerate() {
            writeln!(f, "{i:02X}: {instr:?}")?;
        }
        Ok(())
    }
}

impl<T, E: Engine<T>> Program<T, E> {
    pub fn new(prog: Vec<Instr<T, E>>, init: E::Init) -> Program<T, E> {
        Program { prog, init }
    }

    /// Executes the program. Returns a vector of matches found. For each match, the positions of
    /// all the save locations are stored in a vector
    pub fn exec<I>(&self, input: I) -> Vec<E>
    where
        I: IntoIterator<Item = T>,
    {
        let mut input = input.into_iter().enumerate().peekable();

        // initialize thread lists. The number of threads should be limited by the length of the
        // program, since each instruction either ends a thread (in the case of a `Match`, `Reject`
        // or a failed `Peek` or `Consume` instruction), continues an existing thread (in the case
        // of a successful `Consume`, `Jump`, or `Peek` instruction), or spawns a new thread (in
        // the case of a `Split` or `JSplit` instruction)
        let mut curr = ThreadList::new(self.prog.len());
        let mut next = ThreadList::new(self.prog.len());

        let mut saves = Vec::new();
        let mut prune_list = PruneList::new(self.prog.len());

        // start initial thread at start instruction
        curr.add_thread(
            0,
            0,
            input.peek().map(|(_i, tok)| tok),
            self,
            &mut prune_list,
            E::initialize(&self.init),
        );

        // iterate over tokens of input string
        while let Some((i, tok_i)) = input.next() {
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            for mut th in &mut curr {
                match &self[th.pc] {
                    Instr::Consume(args) => {
                        if th.engine.consume(args, i, &tok_i) {
                            next.add_thread(
                                th.pc + 1,
                                i + 1,
                                input.peek().map(|(_i, tok)| tok),
                                self,
                                &mut prune_list,
                                th.engine,
                            );
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
        }

        // now iterate over remaining threads, to check for pending match instructions
        for th in &mut curr {
            if let Instr::Match = self[th.pc] {
                saves.push(th.engine);
            }
            // anything else is a failed match
        }

        // return the list of saved locations
        saves
    }
}

impl<T, E: Engine<T>> Index<InstrPtr> for Program<T, E> {
    type Output = Instr<T, E>;

    fn index(&self, idx: InstrPtr) -> &Instr<T, E> {
        // allow "one-past-the-end" jumps, resulting in a successful match
        if idx == self.prog.len() {
            &Instr::Match
        } else {
            self.prog.index(idx)
        }
    }
}
