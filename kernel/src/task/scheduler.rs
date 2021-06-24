//! # Scheduler

use alloc::collections::VecDeque;
use core::future::Future;
use core::ptr;
use core::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

use super::Task;

fn nop_raw_waker() -> RawWaker {
    fn nop(_: *const ()) {}
    fn clone(_: *const ()) -> RawWaker {
        nop_raw_waker()
    }

    RawWaker::new(ptr::null(), &RawWakerVTable::new(clone, nop, nop, nop))
}

fn nop_waker() -> Waker {
    unsafe { Waker::from_raw(nop_raw_waker()) }
}

pub struct Scheduler {
    queue: VecDeque<Task>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    pub fn spawn<F>(&mut self, f: F)
    where
        F: Future<Output = ()> + 'static,
    {
        self.queue.push_back(Task::new(f));
    }

    pub fn run(&mut self) {
        while let Some(mut task) = self.queue.pop_front() {
            match task.poll(&mut Context::from_waker(&nop_waker())) {
                Poll::Ready(_) => {}
                Poll::Pending => self.queue.push_back(task),
            }
        }
    }
}
