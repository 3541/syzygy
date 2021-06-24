//! A type which starts out as one thing, and becomes another.

use core::cell::UnsafeCell;
use core::mem::transmute;
use core::sync::atomic::{AtomicU8, Ordering};

use crate::int;
use crate::util::either::*;

/// The state of the transformation.
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

/// A wrapper type which begins as type `I` and may at some point be transformed
/// into `F`.
pub struct Transform<I, F> {
    /// The actual contents.
    inner: UnsafeCell<Either<I, F>>,
    /// The [transformation state](TransformState).
    state: AtomicU8,
}

unsafe impl<I: Send, F: Send> Send for Transform<I, F> {}
unsafe impl<I: Send + Sync, F: Send + Sync> Sync for Transform<I, F> {}

impl<I, F> Transform<I, F> {
    /// Create a Transform containing the given data.
    pub const fn new(initial: I) -> Transform<I, F> {
        Transform {
            inner: UnsafeCell::new(Left(initial)),
            state: AtomicU8::new(TransformState::Initial as u8),
        }
    }

    /// Replace with the given data of the second type.
    pub fn transform(&self, fin: F) {
        let interrupts = int::disable();

        if self.state.compare_exchange(
            TransformState::Initial as u8,
            TransformState::Transforming as u8,
            Ordering::Acquire,
            Ordering::Acquire,
        ) != Ok(TransformState::Initial as u8)
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

    /// Get a reference to the contents.
    pub fn as_ref(&self) -> Either<&I, &F> {
        match self.state.load(Ordering::Acquire).into() {
            TransformState::Initial | TransformState::Final => {
                unsafe { &*self.inner.get() }.as_ref()
            }
            _ => panic!("Tried to borrow a mid-transformation Transform."),
        }
    }

    /// Get a reference to the initial state.
    pub fn get_initial(&self) -> Option<&I> {
        self.as_ref().left()
    }

    /// Get a reference to the final state.
    pub fn get_final(&self) -> Option<&F> {
        self.as_ref().right()
    }

    /// Get a reference to the final state, or transform using the given closure.
    pub fn get_final_or_transform_with(&self, f: impl FnOnce(&I) -> F) -> &F {
        match self.state.load(Ordering::Acquire).into() {
            TransformState::Final => self.get_final().unwrap(),
            _ => {
                self.transform(f(self.get_initial().unwrap()));
                self.get_final().unwrap()
            }
        }
    }
}

impl<T> Transform<T, T> {
    /// Get a reference to the contents.
    pub fn whichever(&self) -> &T {
        self.as_ref().whichever()
    }
}

#[cfg(test)]
mod test {
    use super::Transform;

    #[test]
    fn basic() {
        let t = Transform::new(true);

        assert_eq!(*t.as_ref().unwrap_left(), true);

        t.transform(123);

        assert_eq!(*t.as_ref().unwrap_right(), 123);
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
        let out = t.as_ref().unwrap_right();
    }

    #[test]
    #[should_panic(expected = "unwrap_left on Either::Right.")]
    fn transformed() {
        let t = Transform::new(true);
        t.transform(true);
        let out = t.as_ref().unwrap_left();
    }
}
