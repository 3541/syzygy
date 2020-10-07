pub use Either::{Left, Right};

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn is_left(&self) -> bool {
        match self {
            Left(_) => true,
            Right(_) => false,
        }
    }

    pub fn unwrap_left(self) -> L {
        match self {
            Left(l) => l,
            Right(_) => panic!("unwrap_left on Either::Right."),
        }
    }
}
