/*
 * SPINLOCK: Dumb and simple spinlock.
 *
 * Copyright (c) 2020-2021, 2026 Alex O'Brien <3541@3541.website>
 *
 * This file is part of Syzygy.
 *
 * Syzygy is free software: you can redistribute it and/or modify it under the
 * terms of version 3 the GNU General Public License as published by the Free
 * Software Foundation.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software. If not, see <https://www.gnu.org/licenses/>.
 */

use core::{
    cell::UnsafeCell, hint::spin_loop, ops::{Deref, DerefMut}, sync::atomic::{AtomicBool, Ordering}
};

use crate::sync::int::InterruptDisableGuard;

struct RawSpinlock(AtomicBool);

impl RawSpinlock {
    const fn new() -> RawSpinlock {
        RawSpinlock(AtomicBool::new(false))
    }

    fn lock(&self) {
        while self
            .0
            .compare_exchange_weak(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            while self.0.load(Ordering::Relaxed) {
                spin_loop();
            }
        }
    }

    pub unsafe fn unlock(&self) {
        self.0.store(false, Ordering::Release);
    }
}

pub struct Spinlock<T> {
    lock: RawSpinlock,
    data: UnsafeCell<T>,
}

pub struct SpinlockGuard<'a, T> {
    lock: &'a RawSpinlock,
    data: &'a mut T,
    _interrupts: InterruptDisableGuard,
}

unsafe impl<T: Send> Send for Spinlock<T> {}
unsafe impl<T: Send> Sync for Spinlock<T> {}

impl<T> Spinlock<T> {
    pub const fn new(data: T) -> Self {
        Self {
            lock: RawSpinlock::new(),
            data: UnsafeCell::new(data),
        }
    }

    pub fn lock(&self) -> SpinlockGuard<'_, T> {
        let interrupts = InterruptDisableGuard::new();
        self.lock.lock();

        SpinlockGuard {
            lock: &self.lock,
            data: unsafe { &mut *self.data.get() },
            _interrupts: interrupts,
        }
    }
}

impl<T> Drop for SpinlockGuard<'_, T> {
    fn drop(&mut self) {
        unsafe { self.lock.unlock() };
    }
}

impl<T> Deref for SpinlockGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.data
    }
}

impl<T> DerefMut for SpinlockGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.data
    }
}

#[cfg(test)]
mod test {
    use std::thread;

    use super::Spinlock;

    #[test]
    fn lock_and_release() {
        let lock = Spinlock::new(123);
        assert_eq!(*lock.lock(), 123);
        {
            *lock.lock() = 345;
        }
        assert_eq!(*lock.lock(), 345);
    }

    fn update_counter(counter: &Spinlock<usize>) {
        let mut lock = counter.lock();
        let val = *lock;
        assert_eq!(val, *lock);
        *lock += 1;
        assert_eq!(val + 1, *lock);
        *lock += 1;
        assert_eq!(val + 2, *lock);
    }

    #[test]
    fn exclusion() {
        static COUNTER: Spinlock<usize> = Spinlock::new(0);

        static MAX: usize = 100000;
        static THREADS: usize = 8;

        let mut threads = Vec::with_capacity(THREADS);

        for _ in 0..THREADS {
            threads.push(thread::spawn(|| {
                for _ in 0..MAX {
                    update_counter(&COUNTER)
                }
            }));
        }

        for t in threads {
            let _ = t.join();
        }

        assert_eq!(*COUNTER.lock(), 2 * MAX * THREADS);
    }
}
