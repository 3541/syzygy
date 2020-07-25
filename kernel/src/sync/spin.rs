use core::cell::UnsafeCell;
use core::ops::{Deref, DerefMut};
use core::sync::atomic::{AtomicBool, AtomicIsize, Ordering};

use crate::arch::pause;

pub struct RawSpinLock(AtomicBool);

// This is separated out so it can be used in cases like the scheduler and
// TempPage.
impl RawSpinLock {
    pub const fn new() -> RawSpinLock {
        RawSpinLock(AtomicBool::new(false))
    }

    pub fn lock(&self) {
        while !self.0.compare_and_swap(false, true, Ordering::AcqRel) {
            while !self.0.load(Ordering::Relaxed) {
                pause();
            }
        }
    }

    /// # Safety
    /// The lock should only be unlocked when it has been locked in the first
    /// place.
    pub unsafe fn unlock(&self) {
        self.0.store(false, Ordering::Release);
    }
}

pub struct SpinLock<T: ?Sized> {
    lock: RawSpinLock,
    inner: UnsafeCell<T>,
}

pub struct SpinLockGuard<'a, T: ?Sized + 'a> {
    lock: &'a RawSpinLock,
    inner: &'a mut T,
}

impl<T: ?Sized> SpinLock<T> {
    pub const fn new(data: T) -> SpinLock<T>
    where
        T: Sized,
    {
        SpinLock {
            lock: RawSpinLock::new(),
            inner: UnsafeCell::new(data),
        }
    }

    pub fn lock(&self) -> SpinLockGuard<T> {
        self.lock.lock();
        SpinLockGuard {
            lock: &self.lock,
            inner: unsafe { &mut *self.inner.get() },
        }
    }
}

impl<T: ?Sized> Drop for SpinLockGuard<'_, T> {
    fn drop(&mut self) {
        unsafe { self.lock.unlock() }
    }
}

impl<T: ?Sized> Deref for SpinLockGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<T: ?Sized> DerefMut for SpinLockGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.inner
    }
}

pub struct RwSpinLock<T: ?Sized> {
    state: AtomicIsize,
    inner: UnsafeCell<T>,
}

pub struct RwSpinLockReadGuard<'a, T: ?Sized + 'a> {
    lock: &'a AtomicIsize,
    inner: &'a T,
}

pub struct RwSpinLockWriteGuard<'a, T: ?Sized + 'a> {
    lock: &'a AtomicIsize,
    inner: &'a mut T,
}

impl<T: ?Sized> RwSpinLock<T> {
    pub const fn new(data: T) -> RwSpinLock<T>
    where
        T: Sized,
    {
        RwSpinLock {
            state: AtomicIsize::new(0),
            inner: UnsafeCell::new(data),
        }
    }

    pub fn read(&self) -> RwSpinLockReadGuard<T> {
        loop {
            let previous_state = self.state.fetch_add(1, Ordering::Acquire);

            if previous_state >= 0 {
                // Either no locks or only read locks exist.
                break;
            } else {
                // State was -1 -- a write lock exists.
                self.state.fetch_sub(1, Ordering::Release);

                while self.state.load(Ordering::Relaxed) == -1 {
                    pause();
                }
            }
        }

        RwSpinLockReadGuard {
            lock: &self.state,
            inner: unsafe { &*self.inner.get() },
        }
    }

    pub fn write(&self) -> RwSpinLockWriteGuard<T> {
        loop {
            let previous_state = self.state.fetch_sub(1, Ordering::Acquire);

            if previous_state == 0 {
                // No locks exist.
                break;
            } else {
                // Some kind of lock exists.
                self.state.fetch_add(1, Ordering::Release);

                while self.state.load(Ordering::Relaxed) != 0 {
                    pause();
                }
            }
        }

        RwSpinLockWriteGuard {
            lock: &self.state,
            inner: unsafe { &mut *self.inner.get() },
        }
    }
}

impl<T: ?Sized> Deref for RwSpinLockReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<T: ?Sized> Drop for RwSpinLockReadGuard<'_, T> {
    fn drop(&mut self) {
        self.lock.fetch_sub(1, Ordering::Release);
    }
}

impl<T: ?Sized> Deref for RwSpinLockWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<T: ?Sized> DerefMut for RwSpinLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.inner
    }
}

impl<T: ?Sized> Drop for RwSpinLockWriteGuard<'_, T> {
    fn drop(&mut self) {
        self.lock.fetch_add(1, Ordering::Release);
    }
}
