use alloc::sync::Arc;
use core::sync::atomic::{AtomicUsize, Ordering};

use hashbrown::HashMap;

use spin::{Mutex, MutexGuard, Once, RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::memory::paging::table::{ActiveTopLevelTable, TopLevelTable};
use crate::memory::paging::Pager;
use crate::memory::VirtualRegion;

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct TaskID(usize);

//#[thread_local]
static TASK_ID: AtomicUsize = AtomicUsize::new(0);
static TASK_LIST: Once<RwLock<TaskList>> = Once::new();

pub struct Task {
    pager: Mutex<Pager>,
}

impl Task {
    pub fn pager(&self) -> MutexGuard<Pager> {
        self.pager.lock()
    }
}

pub struct TaskList {
    tasks: HashMap<TaskID, Arc<Task>>,
    next_id: TaskID,
}

impl TaskList {
    fn new() -> Self {
        TaskList {
            tasks: HashMap::new(),
            next_id: TaskID(1),
        }
    }

    fn init(&mut self, table: ActiveTopLevelTable, kernel_region: VirtualRegion) {
        self.tasks.insert(
            TaskID(0),
            Arc::new(Task {
                pager: Mutex::new(Pager::new(TopLevelTable::Active(table), kernel_region)),
            }),
        );
    }

    pub fn current(&self) -> &Arc<Task> {
        &self
            .tasks
            .get(&current_id())
            .expect("Current task is missing.")
    }
}

pub fn current_id() -> TaskID {
    TaskID(TASK_ID.load(Ordering::SeqCst))
}

pub fn task_list() -> RwLockReadGuard<'static, TaskList> {
    TASK_LIST.call_once(|| RwLock::new(TaskList::new())).read()
}

pub fn task_list_mut() -> RwLockWriteGuard<'static, TaskList> {
    TASK_LIST.call_once(|| RwLock::new(TaskList::new())).write()
}

pub fn current_task() -> Arc<Task> {
    task_list().current().clone()
}

pub fn init(table: ActiveTopLevelTable, kernel_region: VirtualRegion) {
    task_list_mut().init(table, kernel_region);
}
