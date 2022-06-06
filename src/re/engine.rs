//! The [`Engine`] trait defines a specific set of matching behaviors.

pub trait Engine<T>: Clone {
    /// The initialization argument type for the engine.
    type Init;
    /// The type for the [`Consume`](super::program::Instr::Consume) instruction.
    type Consume;
    /// The type for the [`Peek`](super::program::Instr::Peek) instruction.
    type Peek;

    /// Initialize a new `Engine`
    fn initialize(args: &Self::Init) -> Self;

    /// Call the [`Consume`](super::program::Instr::Consume) instruction.
    fn consume(&mut self, args: &Self::Consume, index: usize, token: &T) -> bool;

    /// Call the [`Peek`](super::program::Instr::Peek) instruction.
    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&T>) -> bool;
}
