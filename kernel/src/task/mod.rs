use core::sync::atomic::{AtomicUsize, Ordering};

use hashbrown::HashMap;

use spin::{Mutex, MutexGuard, Once, RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::memory::paging::table::{ActiveTopLevelTable, TopLevelTable};
use crate::memory::paging::Pager;
use crate::memory::region::VirtualRegionAllocator;
use crate::memory::VirtualRegion;

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct TaskID(usize);

//#[thread_local]
static TASK_ID: AtomicUsize = AtomicUsize::new(1);
static TASK_LIST: Once<RwLock<TaskList>> = Once::new();

pub struct Task {
    pager: Mutex<Pager>,
}

impl Task {
    pub fn pager(&self) -> MutexGuard<Pager> {
        self.pager.lock()
    }
}

pub struct TaskList(HashMap<TaskID, Task>);

impl TaskList {
    fn new() -> Self {
        TaskList(HashMap::new())
    }

    fn init(&mut self, table: ActiveTopLevelTable, region: VirtualRegion) {
        self.0.insert(
            TaskID(0),
            Task {
                pager: Mutex::new(Pager::new(TopLevelTable::Active(table), region)),
            },
        );
    }

    pub fn current(&self) -> &Task {
        &self.0.get(&current_id()).expect("Current task is missing.")
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

pub fn init(table: ActiveTopLevelTable, region: VirtualRegion) {
    task_list_mut().init(table, region);
}
