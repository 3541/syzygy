//! # Scheduler

use alloc::collections::VecDeque;
use alloc::sync::Arc;
use alloc::task::Wake;
use core::future::Future;
use core::task::{Context, Poll, Waker};

use hashbrown::HashMap;

use super::{Task, TaskId};
use crate::int;
use crate::util::sync::{self, OnceCell, Spinlock};

type TaskQueue = Arc<Spinlock<VecDeque<TaskId>>>;

struct SchedWaker {
    id: TaskId,
    queue: TaskQueue,
}

impl SchedWaker {
    fn new(id: TaskId, queue: TaskQueue) -> Waker {
        Waker::from(Arc::new(Self { id, queue }))
    }

    pub fn awaken(&self) {
        self.queue.lock().push_back(self.id);
    }
}

impl Wake for SchedWaker {
    fn wake(self: Arc<Self>) {
        self.awaken()
    }

    fn wake_by_ref(self: &Arc<Self>) {
        self.awaken()
    }
}

struct TaskEntry {
    task: Spinlock<Task>,
    waker: OnceCell<Waker>,
}

impl TaskEntry {
    fn new(task: Task) -> Self {
        Self {
            task: Spinlock::new(task),
            waker: OnceCell::new(),
        }
    }
}

static SCHEDULER: OnceCell<Scheduler> = OnceCell::new();

pub struct Scheduler {
    tasks: Spinlock<HashMap<TaskId, Arc<TaskEntry>>>,
    queue: TaskQueue,
}

impl Scheduler {
    pub fn the() -> &'static Self {
        &*SCHEDULER
    }

    fn new() -> Self {
        Self {
            tasks: Spinlock::new(HashMap::new()),
            queue: Arc::new(Spinlock::new(VecDeque::with_capacity(100))),
        }
    }

    pub fn spawn<F>(&self, f: F)
    where
        F: Future<Output = ()> + Send + 'static,
    {
        let task = Task::new(f);
        let id = task.id;

        let dup = self.tasks.lock().insert(id, Arc::new(TaskEntry::new(task)));
        assert!(!dup.is_some());
        self.queue.lock().push_back(id);
    }

    fn run_queue(&self) {
        let Self { tasks, queue } = self;

        while let Some(id) = queue.lock().pop_front() {
            fn poll(sched: &Scheduler, id: TaskId) -> Poll<()> {
                let task = match sched.tasks.lock().get(&id) {
                    Some(t) => Arc::clone(t),
                    None => return Poll::Pending,
                };

                let mut ctx = Context::from_waker(
                    task.waker
                        .borrow_or_init_with(|| SchedWaker::new(id, Arc::clone(&sched.queue))),
                );

                let res = task.task.lock().poll(&mut ctx);
                res
            }

            match poll(self, id) {
                Poll::Ready(()) => {
                    let _ = tasks.lock().remove(&id);
                }
                Poll::Pending => {}
            }
        }
    }

    pub fn run(&self) -> ! {
        loop {
            self.run_queue();

            let interrupts = int::disable();
            if interrupts && self.queue.lock().is_empty() {
                int::enable_and_halt();
            } else {
                sync::pause();
            }
        }
    }
}

pub fn init() {
    SCHEDULER.init(Scheduler::new());
}
