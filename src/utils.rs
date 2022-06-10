//! Utility types.

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
        match self {
            BooleanExpr::Value(x) => BooleanExpr::Value(func(x)),
            BooleanExpr::And(xs) => BooleanExpr::And(xs.iter().map(|x| x.map(&mut func)).collect()),
            BooleanExpr::Or(xs) => BooleanExpr::Or(xs.iter().map(|x| x.map(&mut func)).collect()),
            BooleanExpr::Not(x) => x.map(func).not(),
            BooleanExpr::True => BooleanExpr::True,
            BooleanExpr::False => BooleanExpr::False,
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
                BooleanExpr::Not(_) => {
                    // temporary, so that we can move out of `self`
                    let this = std::mem::replace(self, BooleanExpr::False).not();
                    *self = this;
                    self.coalesce();
                }
                _ => {}
            }
        }
    }
}
