//! The [`Engine`] trait defines a specific set of matching behaviors.

pub trait Engine<T>: Clone {
    /// The initialization argument type for the engine.
    type Init: Clone;
    /// The type for the [`Consume`](super::program::Instr::Consume) instruction.
    type Consumer: for<'t> Check<Self, &'t T>;
    /// The type for the [`Peek`](super::program::Instr::Peek) instruction.
    type Peeker: for<'t> Check<Self, Option<&'t T>>;

    /// Initialize a new `Engine`
    fn initialize(args: Self::Init) -> Self;
}

pub trait Check<E, T> {
    fn check(&self, engine: &mut E, index: usize, token: T) -> bool;
}
