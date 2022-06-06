//! Pattern matching based on regular expressions (but with a bit more power).
pub mod engine;
pub mod program;
mod prune;

#[cfg(test)]
mod tests;
