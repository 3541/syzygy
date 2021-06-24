//! # Multitasking

mod scheduler;

use alloc::boxed::Box;
use core::future::Future;
use core::pin::Pin;
use core::task::{Context, Poll};

use scheduler::Scheduler;

make_id!(TaskId);

pub struct Task {
    id: TaskId,
    inner: Pin<Box<dyn Future<Output = ()> + Send>>,
}

impl Task {
    fn new<F>(f: F) -> Self
    where
        F: Future<Output = ()> + Send + 'static,
    {
        Self {
            id: TaskId::new(),
            inner: Box::pin(f),
        }
    }

    fn poll(&mut self, ctx: &mut Context) -> Poll<()> {
        self.inner.as_mut().poll(ctx)
    }
}

pub fn init<F>(f: F) -> !
where
    F: Future<Output = ()> + Send + 'static,
{
    scheduler::init();
    let s = Scheduler::the();
    s.spawn(f);
    s.run()
}
