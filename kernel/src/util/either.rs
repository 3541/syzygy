//! A type-safe union.

pub use Either::{Left, Right};

/// A type-safe union. Holds either `Left(L)` or `Right(R)`. Akin to
/// `std::variant` in C++.
#[derive(Copy, Clone)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    /// Take variants by reference.
    pub fn as_ref(&self) -> Either<&L, &R> {
        match self {
            Left(ref l) => Left(l),
            Right(ref r) => Right(r),
        }
    }

    /// Assert that the contained value is the left one and return it.
    pub fn unwrap_left(self) -> L {
        match self {
            Left(l) => l,
            Right(_) => panic!("unwrap_left on Either::Right."),
        }
    }

    /// Assert that the contained value is the right one and return it.
    pub fn unwrap_right(self) -> R {
        match self {
            Left(_) => panic!("unwrap_right on Either::Left."),
            Right(r) => r,
        }
    }

    /// Get the left value if it is present.
    pub fn left(self) -> Option<L> {
        match self {
            Left(l) => Some(l),
            Right(_) => None,
        }
    }

    /// Get the right value if it is present.
    pub fn right(self) -> Option<R> {
        match self {
            Left(_) => None,
            Right(r) => Some(r),
        }
    }
}

impl<T> Either<T, T> {
    /// Return whichever variant is present.
    pub fn whichever(self) -> T {
        match self {
            Left(l) => l,
            Right(r) => r,
        }
    }
}
