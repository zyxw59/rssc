//! The [`Engine`] trait defines a specific set of matching behaviors.

use std::hash::Hash;

pub trait Engine: Hash + Clone {
    /// The type of token the engine operates on.
    type Token;
    /// The type for the [`Consume`](super::program::Instr::Consume) instruction.
    type Consume;
    /// The type for the [`Peek`](super::program::Instr::Peek) instruction.
    type Peek;

    /// Call the [`Consume`](super::program::Instr::Consume) instruction.
    fn consume(&mut self, args: &Self::Consume, index: usize, token: &Self::Token) -> bool;

    /// Call the [`Peek`](super::program::Instr::Peek) instruction.
    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&Self::Token>) -> bool;
}
