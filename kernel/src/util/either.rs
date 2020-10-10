pub use Either::{Left, Right};

#[derive(Copy, Clone)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn as_ref(&self) -> Either<&L, &R> {
        match self {
            Left(ref l) => Left(l),
            Right(ref r) => Right(r),
        }
    }

    pub fn unwrap_left(self) -> L {
        match self {
            Left(l) => l,
            Right(_) => panic!("unwrap_left on Either::Right."),
        }
    }

    pub fn unwrap_right(self) -> R {
        match self {
            Left(_) => panic!("unwrap_right on Either::Left."),
            Right(r) => r,
        }
    }

    pub fn right(self) -> Option<R> {
        match self {
            Left(_) => None,
            Right(r) => Some(r),
        }
    }
}

impl<T> Either<T, T> {
    pub fn whichever(&self) -> &T {
        match self.as_ref() {
            Left(l) => l,
            Right(r) => r,
        }
    }
}
