//! # Multitasking

mod scheduler;

use alloc::boxed::Box;
use core::future::Future;
use core::pin::Pin;
use core::task::{Context, Poll};

use log_crate::info;

pub struct Task(Pin<Box<dyn Future<Output = ()>>>);

impl Task {
    fn new<F>(f: F) -> Self
    where
        F: Future<Output = ()> + 'static,
    {
        Self(Box::pin(f))
    }

    fn poll(&mut self, ctx: &mut Context) -> Poll<()> {
        self.0.as_mut().poll(ctx)
    }
}

pub fn init() {
    /*    let mut sched = scheduler::Scheduler::new();

    for _ in 0..1000 {
        sched.spawn(increment_counter())
    }

    sched.run();

    info!("Result is: {}.", *COUNTER.try_lock().unwrap());*/
}
