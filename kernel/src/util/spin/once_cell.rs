use core::cell::UnsafeCell;
use core::mem::transmute;
use core::ops::Deref;
use core::sync::atomic::{AtomicU8, Ordering};

use crate::int;

#[repr(u8)]
enum OnceCellState {
    Empty = 0,
    Initializing = 1,
    Initialized = 2,
}

impl From<u8> for OnceCellState {
    fn from(value: u8) -> OnceCellState {
        if value > 2 {
            panic!("Invalid OnceCellState.");
        }

        unsafe { transmute(value) }
    }
}

pub struct OnceCell<T> {
    inner: UnsafeCell<Option<T>>,
    state: AtomicU8,
}

unsafe impl<T> Sync for OnceCell<T> {}

impl<T> OnceCell<T> {
    pub const fn new() -> OnceCell<T> {
        OnceCell {
            inner: UnsafeCell::new(None),
            state: AtomicU8::new(OnceCellState::Empty as u8),
        }
    }

    // Initialize the cell. Panics if double-initialized.
    pub fn init(&self, data: T) {
        let interrupts = int::disable();

        if self.state.compare_and_swap(
            OnceCellState::Empty as u8,
            OnceCellState::Initializing as u8,
            Ordering::Acquire,
        ) != OnceCellState::Empty as u8
        {
            panic!("Tried to initialize OnceCell twice.");
        }

        unsafe { *self.inner.get() = Some(data) };

        self.state
            .store(OnceCellState::Initialized as u8, Ordering::Release);

        int::set(interrupts);
    }

    pub fn borrow(&self) -> Option<&T> {
        match self.state.load(Ordering::Acquire).into() {
            OnceCellState::Initialized => unsafe { &*self.inner.get() }.as_ref(),
            _ => None,
        }
    }
}

impl<T> Deref for OnceCell<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.borrow()
            .expect("Tried to dereference an uninitialized OnceCell.")
    }
}
