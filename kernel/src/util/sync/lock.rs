//! # Async lock

use alloc::vec::Vec;
use core::future::Future;
use core::pin::Pin;
use core::sync::atomic::{AtomicBool, Ordering};
use core::task::{Context, Poll, Waker};

use super::Spinlock;

// In the interest of preventing reoccurrences of
// fe835cf48959fa762962f3ab7518cddf35cf9e39, the meaning of the bool is as
// follows:
//     * false => UNLOCKED
//     * true  => LOCKED
/// An async-friendly lock.
pub struct Lock {
    lock: AtomicBool,
    waiting: Spinlock<Vec<Waker>>,
}

/// The future generated when locking.
pub struct LockRequest<'a> {
    lock: &'a Lock,
    woke: bool,
}

impl Lock {
    pub const fn new() -> Self {
        Self {
            lock: AtomicBool::new(false),
            waiting: Spinlock::new(Vec::new()),
        }
    }

    pub fn try_lock(&self) -> bool {
        self.lock
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    }

    pub fn lock(&self) -> LockRequest {
        LockRequest {
            lock: self,
            woke: false,
        }
    }

    pub unsafe fn unlock(&self) {
        self.lock.store(false, Ordering::Release);

        for waker in self.waiting.lock().drain(0..) {
            waker.wake()
        }
    }

    fn wait(&self, w: Waker) {
        self.waiting.lock().push(w)
    }

    fn unwait(&self, w: &Waker) {
        let mut waiting = self.waiting.lock();
        for i in 0..waiting.len() {
            if waiting[i].will_wake(w) {
                waiting.remove(i);
                break;
            }
        }
    }
}

impl Future for LockRequest<'_> {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context) -> Poll<()> {
        let waker = ctx.waker();
        if self.lock.try_lock() {
            if self.woke {
                self.lock.unwait(waker);
            }
            return Poll::Ready(());
        }

        self.lock.wait(waker.clone());
        if self.lock.try_lock() {
            self.lock.unwait(waker);
            Poll::Ready(())
        } else {
            self.woke = true;
            Poll::Pending
        }
    }
}
