//! Utility types.

use std::{cmp, fmt};

/// A boolean expression, consisting of nested And, Or, and Not operators.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BooleanExpr<T> {
    /// A terminal boolean value.
    Value(T),
    /// Evaluates to `true` if all sub-expressions evaluate to `true`. If there are no
    /// sub-expressions, it evaluates to `true`.
    And(Vec<Self>),
    /// Evaluates to `true` if any sub-expression evaluates to `true`. If there are no
    /// sub-expressions, it evaluates to `false`.
    Or(Vec<Self>),
    /// Evaluates to `true` if the sub-expression evaluates to `false` (and vice-versa).
    Not(Box<Self>),
    /// Always evaluates to `true`.
    True,
    /// Always evaluates to `false`.
    False,
}

impl<T> BooleanExpr<T> {
    #[must_use = "`not` produces a new `BooleanExpr`"]
    pub fn not(self) -> Self {
        match self {
            BooleanExpr::Not(x) => *x,
            BooleanExpr::True => BooleanExpr::False,
            BooleanExpr::False => BooleanExpr::True,
            _ => BooleanExpr::Not(Box::new(self)),
        }
    }

    /// Applies the specified function to each sub-expression.
    pub fn map<F, U>(&self, mut func: F) -> BooleanExpr<U>
    where
        F: FnMut(&T) -> U,
    {
        match self.try_map(|x| Ok::<_, std::convert::Infallible>(func(x))) {
            Ok(x) => x,
            Err(e) => match e {},
        }
    }

    /// Applies the specified function to each sub-expression, returning any `Err` encountered.
    pub fn try_map<F, U, E>(&self, mut func: F) -> Result<BooleanExpr<U>, E>
    where
        F: FnMut(&T) -> Result<U, E>,
    {
        self.try_map_internal(&mut func)
    }

    fn try_map_internal<F, U, E>(&self, func: &mut F) -> Result<BooleanExpr<U>, E>
    where
        F: FnMut(&T) -> Result<U, E>,
    {
        match self {
            BooleanExpr::Value(x) => func(x).map(BooleanExpr::Value),
            BooleanExpr::And(xs) => xs
                .iter()
                .map(|x| x.try_map_internal(func))
                .collect::<Result<_, _>>()
                .map(BooleanExpr::And),
            BooleanExpr::Or(xs) => xs
                .iter()
                .map(|x| x.try_map_internal(func))
                .collect::<Result<_, _>>()
                .map(BooleanExpr::Or),
            BooleanExpr::Not(x) => x.try_map_internal(func).map(BooleanExpr::not),
            BooleanExpr::True => Ok(BooleanExpr::True),
            BooleanExpr::False => Ok(BooleanExpr::False),
        }
    }

    /// Evaluates each sub-expression using the specified function, and returns the result.
    pub fn evaluate(&self, mut func: impl FnMut(&T) -> bool) -> bool {
        match self {
            BooleanExpr::Value(x) => func(x),
            BooleanExpr::And(xs) => xs.iter().all(|x| x.evaluate(&mut func)),
            BooleanExpr::Or(xs) => xs.iter().any(|x| x.evaluate(&mut func)),
            BooleanExpr::Not(x) => !x.evaluate(func),
            BooleanExpr::True => true,
            BooleanExpr::False => false,
        }
    }

    /// Returns whether the function is trivially true.
    fn is_true(&self) -> bool {
        match self {
            BooleanExpr::And(xs) => xs.iter().all(BooleanExpr::is_true),
            BooleanExpr::Or(xs) => xs.iter().any(BooleanExpr::is_true),
            BooleanExpr::Not(x) => x.is_false(),
            BooleanExpr::Value(_) => false,
            BooleanExpr::True => true,
            BooleanExpr::False => false,
        }
    }

    /// Returns whether the function is trivially false.
    fn is_false(&self) -> bool {
        match self {
            BooleanExpr::And(xs) => xs.iter().any(BooleanExpr::is_false),
            BooleanExpr::Or(xs) => xs.iter().all(BooleanExpr::is_false),
            BooleanExpr::Not(x) => x.is_true(),
            BooleanExpr::Value(_) => false,
            BooleanExpr::True => false,
            BooleanExpr::False => true,
        }
    }

    pub fn coalesce(&mut self) {
        if self.is_true() {
            *self = BooleanExpr::True;
        } else if self.is_false() {
            *self = BooleanExpr::False;
        } else {
            match self {
                BooleanExpr::And(xs) => {
                    let mut xs = xs
                        .drain(..)
                        .filter_map(|mut x| {
                            if x.is_true() {
                                None
                            } else {
                                x.coalesce();
                                Some(x)
                            }
                        })
                        .collect::<Vec<_>>();
                    *self = if xs.len() == 1 {
                        xs.pop().unwrap()
                    } else {
                        BooleanExpr::And(xs)
                    };
                }
                BooleanExpr::Or(xs) => {
                    let mut xs = xs
                        .drain(..)
                        .filter_map(|mut x| {
                            if x.is_false() {
                                None
                            } else {
                                x.coalesce();
                                Some(x)
                            }
                        })
                        .collect::<Vec<_>>();
                    *self = if xs.len() == 1 {
                        xs.pop().unwrap()
                    } else {
                        BooleanExpr::Or(xs)
                    }
                }
                BooleanExpr::Not(x) => {
                    // temporary, so that we can move out of `self`
                    let mut new_x = std::mem::replace(&mut **x, BooleanExpr::False);
                    new_x.coalesce();
                    *self = new_x.not();
                }
                _ => {}
            }
        }
    }
}

impl<T: fmt::Display> fmt::Display for BooleanExpr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BooleanExpr::Value(x) => fmt::Display::fmt(x, f),
            BooleanExpr::And(xs) => {
                if f.alternate() {
                    f.write_str("&(")?;
                    if let Some((first, rest)) = xs.split_first() {
                        fmt::Display::fmt(first, f)?;
                        for x in rest {
                            write!(f, " {x}")?;
                        }
                    }
                    f.write_str(")")
                } else {
                    f.write_str("And(\n")?;
                    for x in xs {
                        writeln!(f, "{x},")?;
                    }
                    f.write_str(")")
                }
            }
            BooleanExpr::Or(xs) => {
                if f.alternate() {
                    f.write_str("|(")?;
                    if let Some((first, rest)) = xs.split_first() {
                        fmt::Display::fmt(first, f)?;
                        for x in rest {
                            write!(f, " {x}")?;
                        }
                    }
                    f.write_str(")")
                } else {
                    f.write_str("Or(\n")?;
                    for x in xs {
                        writeln!(f, "{x},")?;
                    }
                    f.write_str(")")
                }
            }
            BooleanExpr::Not(x) => {
                if f.alternate() {
                    write!(f, "!{x}")
                } else {
                    f.write_str("Not(\n")?;
                    fmt::Display::fmt(x, f)?;
                    f.write_str(")")
                }
            }
            BooleanExpr::True => f.write_str("True"),
            BooleanExpr::False => f.write_str("False"),
        }
    }
}

/// A wrapper around a [`Vec`] which sorts by length first.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SortByLen<T>(pub Vec<T>);

impl<T: cmp::Ord> cmp::Ord for SortByLen<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0
            .len()
            .cmp(&other.0.len())
            .then_with(|| self.0.cmp(&other.0))
    }
}

impl<T: cmp::Ord> cmp::PartialOrd for SortByLen<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::BooleanExpr;

    #[test]
    fn coalesce() {
        let mut x = BooleanExpr::Value("").not();
        println!("{x}");
        x.coalesce();
        assert_eq!(x, BooleanExpr::Not(Box::new(BooleanExpr::Value(""))));
    }
}
