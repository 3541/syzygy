use core::cell::UnsafeCell;
use core::mem::transmute;
use core::sync::atomic::{AtomicU8, Ordering};

use crate::int;
use crate::util::either::*;

#[repr(u8)]
enum TransformState {
    Initial = 0,
    Transforming = 1,
    Final = 2,
}

impl From<u8> for TransformState {
    fn from(value: u8) -> TransformState {
        if value > TransformState::Final as u8 {
            panic!("Invalid TransformState {}.", value);
        }

        unsafe { transmute(value) }
    }
}

pub struct Transform<I, F> {
    inner: UnsafeCell<Either<I, F>>,
    state: AtomicU8,
}

unsafe impl<I: Send, F: Send> Send for Transform<I, F> {}
unsafe impl<I: Send + Sync, F: Send + Sync> Sync for Transform<I, F> {}

impl<I, F> Transform<I, F> {
    pub const fn new(initial: I) -> Transform<I, F> {
        Transform {
            inner: UnsafeCell::new(Left(initial)),
            state: AtomicU8::new(TransformState::Initial as u8),
        }
    }

    pub fn transform(&self, fin: F) {
        let interrupts = int::disable();

        if self.state.compare_and_swap(
            TransformState::Initial as u8,
            TransformState::Transforming as u8,
            Ordering::Acquire,
        ) != TransformState::Initial as u8
        {
            panic!("Tried to double-transform Transform.");
        }

        unsafe { *self.inner.get() = Right(fin) };

        self.state
            .store(TransformState::Final as u8, Ordering::Release);

        if interrupts {
            int::enable();
        }
    }

    pub fn borrow(&self) -> Either<&I, &F> {
        match self.state.load(Ordering::Acquire).into() {
            TransformState::Initial | TransformState::Final => {
                unsafe { &*self.inner.get() }.as_ref()
            }
            _ => panic!("Tried to borrow a mid-transformation Transform."),
        }
    }

    pub fn borrow_initial(&self) -> &I {
        self.borrow().unwrap_left()
    }

    pub fn borrow_final(&self) -> &F {
        self.borrow().unwrap_right()
    }
}

impl<T> Transform<T, T> {
    pub fn whichever(&self) -> &T {
        self.borrow().whichever()
    }
}

#[cfg(test)]
mod test {
    use super::Transform;

    #[test]
    fn basic() {
        let t = Transform::new(true);

        assert_eq!(*t.borrow().unwrap_left(), true);

        t.transform(123);

        assert_eq!(*t.borrow().unwrap_right(), 123);
    }

    #[test]
    #[should_panic(expected = "Tried to double-transform Transform.")]
    fn double_transform() {
        let t = Transform::new(true);
        t.transform(true);
        t.transform(true);
    }

    #[test]
    #[should_panic(expected = "unwrap_right on Either::Left.")]
    fn untransformed() {
        let t = Transform::<bool, bool>::new(true);
        let out = t.borrow_final();
    }

    #[test]
    #[should_panic(expected = "unwrap_left on Either::Right.")]
    fn transformed() {
        let t = Transform::new(true);
        t.transform(true);
        let out = t.borrow_initial();
    }
}
