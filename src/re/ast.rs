use std::collections::HashSet;

use super::{token::Token, program};

/// A regular expression
#[derive(Debug)]
pub enum Regex<T: Token> {
    /// Matches an empty string, i.e. zero tokens.
    Empty,
    /// Matches a literal string of tokens.
    Literal(Vec<T>),
    /// Matches any single token.
    Any,
    /// Matches a word boundary.
    WordBoundary,
    /// Matches a single token from a set of tokens.
    Set(HashSet<T>),
    /// Matches a repeating pattern
    Repeat(Box<Regex<T>>, Repeater),
    /// Generates a capturing group
    Capture(Box<Regex<T>>),
    /// Matches a concatenation of two patterns
    Concat(Vec<Regex<T>>),
    /// Matches one of multiple patterns
    Alternate(Vec<Regex<T>>),
}

impl<T: Token> Regex<T> {
    /// Compiles a regular expression into a program to be executed.
    pub fn compile(self) -> program::Program<T> {
        use self::program::Instr::*;
        // first, match /.*?/ to find earliest start of match, and save match point
        let mut v = vec![JSplit(3), Any, Jump(0), Save(0)];
        let mut num_captures = 0;
        self.compile_partial(&mut v, &mut num_captures);
        // save end of match
        v.push(Save(1));
        // finish
        v.push(Match);

        // construct final program
        program::Program::new(v, 2 + num_captures * 2)
    }

    fn compile_partial(self, v: &mut Vec<program::Instr<T>>, num_captures: &mut usize) {
        use self::program::Instr::*;
        match self {
            Regex::Empty => {}
            Regex::Literal(toks) => for t in toks {
                v.push(Token(t));
            },
            Regex::Any => {
                v.push(Any);
            }
            Regex::WordBoundary => {
                v.push(WordBoundary);
            }
            Regex::Set(set) => {
                v.push(Set(set));
            }
            Regex::Repeat(e, rep) => {
                use self::Repeater::*;
                match rep {
                    ZeroOrOne(greedy) => {
                        // store current length of vector, i.e. index of `Split` instruction
                        let i = v.len();
                        // dummy instruction
                        v.push(Split(0));
                        // compile `e`
                        e.compile_partial(v, num_captures);
                        if greedy {
                            // prefer not to skip to end
                            v[i] = Split(v.len());
                        } else {
                            // prefer to skip to end
                            v[i] = JSplit(v.len());
                        }
                    }
                    ZeroOrMore(greedy) => {
                        // save current pc
                        let start = v.len();
                        // dummy instruction
                        v.push(Split(0));
                        // compile `e`
                        e.compile_partial(v, num_captures);
                        // repeat
                        v.push(Jump(start));
                        if greedy {
                            // prefer not to skip to end
                            v[start] = Split(v.len());
                        } else {
                            // prefer to skip to end
                            v[start] = JSplit(v.len());
                        }
                    }
                    OneOrMore(greedy) => {
                        // save current pc
                        let start = v.len();
                        // compile `e`
                        // here we're just putting it directly on the stack
                        e.compile_partial(v, num_captures);
                        if greedy {
                            // prefer to repeat
                            v.push(JSplit(start));
                        } else {
                            // prefer not to repeat, i.e. prefer to continue
                            v.push(Split(start));
                        }
                    }
                }
            }
            Regex::Capture(e) => {
                // increment
                *num_captures += 1;
                // save current value of `num_captures`, incase `e` has any captures
                let n = *num_captures;
                // save begining of capture
                v.push(Save(n * 2));
                // match `e`
                e.compile_partial(v, num_captures);
                // save end of capture
                v.push(Save(n * 2 + 1));
            }
            Regex::Concat(es) => for e in es {
                e.compile_partial(v, num_captures);
            },
            Regex::Alternate(mut es) => {
                // this can definitely be optimized by using maps
                // for now, a simple iteration over alternatives

                // loop over alternatives
                // save last branch, because it doesn't have a `Split` instruction before it, so it
                // can't be handled in the loop. while we're at it, check if vector is empty
                if let Some(last) = es.pop() {
                    // store indices of split instructions
                    let mut splits = Vec::new();
                    // store indices of jump instructions
                    let mut jumps = Vec::new();

                    for e in es {
                        // save split location
                        splits.push(v.len());
                        // dummy instruction
                        v.push(Split(0));
                        // compile `e`
                        e.compile_partial(v, num_captures);
                        // store jump location
                        jumps.push(v.len());
                        // dummy instruction
                        v.push(Jump(0));
                    }

                    // now, set splits correctly -- each split should point to the next one, except
                    // for the last, which should point where we are now.
                    splits.push(v.len());
                    for (i, j) in splits.iter().zip(splits[1..].iter()) {
                        v[*i] = Split(*j);
                    }

                    // compile `last`
                    last.compile_partial(v, num_captures);

                    // now, set jumps correctly -- all jumps should point to where we are now.
                    let pc = v.len();
                    for i in jumps {
                        v[i] = Jump(pc);
                    }
                }
                // if the `if let` failed, it means that `es` was empty, so we should do nothing
            }
        }
    }
}

/// The type of a repetition. In each enum variant, the one argument determines whether or not the
/// repetition should be greedy, i.e. preferring to match longer strings over shorter strings.
#[derive(Debug)]
pub enum Repeater {
    /// Matches zero or one instances, i.e. `?` or `??`
    ZeroOrOne(bool),
    /// Matches zero or more instances, i.e. `*` or `*?`
    ZeroOrMore(bool),
    /// Matches one or more instances, i.e. `+` or `+?`
    OneOrMore(bool),
}
