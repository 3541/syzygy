//! Spinlocks.

use core::cell::UnsafeCell;
use core::ops::{Deref, DerefMut};
use core::sync::atomic::{AtomicBool, Ordering};

use super::arch::pause;
use crate::int;

// In the interest of preventing reoccurrences of
// fe835cf48959fa762962f3ab7518cddf35cf9e39, the meaning of the bool is as
// follows:
//     * false => UNLOCKED
//     * true  => LOCKED
/// A simple spinlock.
pub struct RawSpinlock(AtomicBool);

impl RawSpinlock {
    pub const fn new() -> RawSpinlock {
        RawSpinlock(AtomicBool::new(false))
    }

    /// Spin until the lock can be acquired.
    pub fn lock(&self) {
        while self
            .0
            .compare_exchange_weak(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            while self.0.load(Ordering::Relaxed) {
                pause();
            }
        }
    }

    /// Release the lock.
    /// # Safety
    /// Should only be called when the lock was previously held.
    pub unsafe fn unlock(&self) {
        self.0.store(false, Ordering::Release);
    }
}

/// An RAII container guarded by a spinlock.
pub struct Spinlock<T> {
    lock: RawSpinlock,
    /// The actual contents.
    inner: UnsafeCell<T>,
}

/// A reference to the contents of a spinlock.
pub struct SpinlockGuard<'a, T: 'a> {
    lock: &'a RawSpinlock,
    inner: &'a mut T,
    interrupts: bool,
}

unsafe impl<T: Send> Send for Spinlock<T> {}
unsafe impl<T: Send> Sync for Spinlock<T> {}

impl<T> Spinlock<T> {
    /// Create a new spinlock containing the given data.
    pub const fn new(data: T) -> Spinlock<T> {
        Spinlock {
            lock: RawSpinlock::new(),
            inner: UnsafeCell::new(data),
        }
    }

    /// Acquire the lock and a reference to the contents.
    pub fn lock(&self) -> SpinlockGuard<T> {
        let interrupts = int::disable();
        self.lock.lock();

        SpinlockGuard {
            lock: &self.lock,
            inner: unsafe { &mut *self.inner.get() },
            interrupts,
        }
    }
}

impl<T> Drop for SpinlockGuard<'_, T> {
    /// Release the lock.
    fn drop(&mut self) {
        unsafe { self.lock.unlock() };
        if self.interrupts {
            int::enable();
        }
    }
}

impl<T> Deref for SpinlockGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<T> DerefMut for SpinlockGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.inner
    }
}
