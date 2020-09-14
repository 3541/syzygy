use core::cell::UnsafeCell;
use core::ops::{Deref, DerefMut};
use core::sync::atomic::{AtomicBool, Ordering};

use super::arch::pause;
use crate::int;

pub struct RawSpinlock(AtomicBool);

impl RawSpinlock {
    pub const fn new() -> RawSpinlock {
        RawSpinlock(AtomicBool::new(false))
    }

    pub fn lock(&self) {
        while !self.0.compare_and_swap(false, true, Ordering::AcqRel) {
            while !self.0.load(Ordering::Relaxed) {
                pause();
            }
        }
    }

    pub unsafe fn unlock(&self) {
        self.0.store(false, Ordering::Release);
    }
}

pub struct Spinlock<T> {
    lock: RawSpinlock,
    inner: UnsafeCell<T>,
}

pub struct SpinlockGuard<'a, T: 'a> {
    lock: &'a RawSpinlock,
    inner: &'a mut T,
    interrupts: bool,
}

unsafe impl<T> Sync for Spinlock<T> {}

impl<T> Spinlock<T> {
    pub const fn new(data: T) -> Spinlock<T> {
        Spinlock {
            lock: RawSpinlock::new(),
            inner: UnsafeCell::new(data),
        }
    }

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