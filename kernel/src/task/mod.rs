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

use crate::util::sync::Mutex;

static COUNTER: Mutex<u32> = Mutex::new(0);

async fn increment_counter() {
    *COUNTER.lock().await += 1
}

pub fn init() {
    let mut sched = scheduler::Scheduler::new();

    for _ in 0..1000 {
        sched.spawn(increment_counter())
    }

    sched.run();

    info!("Result is: {}.", *COUNTER.try_lock().unwrap());
}
