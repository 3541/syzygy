//! # Mutex

use core::cell::UnsafeCell;
use core::ops::{Deref, DerefMut, Drop};

use super::lock::Lock;

/// An async-aware mutex.
pub struct Mutex<T> {
    lock: Lock,
    inner: UnsafeCell<T>,
}

pub struct MutexGuard<'a, T: 'a> {
    lock: &'a Lock,
    inner: &'a mut T,
}

unsafe impl<T: Send> Send for Mutex<T> {}
unsafe impl<T: Send> Sync for Mutex<T> {}

impl<T> Mutex<T> {
    /// Create a new mutex containing the given data.
    pub const fn new(data: T) -> Self {
        Self {
            lock: Lock::new(),
            inner: UnsafeCell::new(data),
        }
    }

    /// Acquire the lock.
    #[inline]
    pub async fn lock(&self) -> MutexGuard<'_, T> {
        self.lock.lock().await;
        MutexGuard {
            lock: &self.lock,
            inner: unsafe { &mut *self.inner.get() },
        }
    }

    /// Try to acquire the lock, synchronously.
    pub fn try_lock(&self) -> Option<MutexGuard<'_, T>> {
        if self.lock.try_lock() {
            Some(MutexGuard {
                lock: &self.lock,
                inner: unsafe { &mut *self.inner.get() },
            })
        } else {
            None
        }
    }
}

impl<T> Drop for MutexGuard<'_, T> {
    /// Release the lock.
    fn drop(&mut self) {
        unsafe { self.lock.unlock() }
    }
}

impl<T> Deref for MutexGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner
    }
}

impl<T> DerefMut for MutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.inner
    }
}
