use alloc::sync::Arc;
use core::mem::size_of;
use core::ptr;
use core::sync::atomic::{AtomicUsize, Ordering};

use hashbrown::HashMap;

use spin::{Mutex, MutexGuard, Once, RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::arch::register;
use crate::memory::paging::table::{ActiveTopLevelTable, EntryFlags, TopLevelTable};
use crate::memory::paging::Pager;
use crate::memory::region::{VirtualRegion, VirtualRegionAllocator};
use crate::memory::{Address, VirtualAddress};

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct TaskID(usize);

#[thread_local]
static TASK_ID: AtomicUsize = AtomicUsize::new(0);
static TASK_LIST: Once<RwLock<TaskList>> = Once::new();

pub struct Task {
    pager: Mutex<Pager>,
    _kernel_tls: VirtualRegion,
}

impl Task {
    pub fn pager(&self) -> MutexGuard<Pager> {
        self.pager.lock()
    }
}

pub struct TaskList {
    tasks: HashMap<TaskID, Arc<Task>>,
    _next_id: TaskID,
}

impl TaskList {
    fn new() -> Self {
        TaskList {
            tasks: HashMap::new(),
            _next_id: TaskID(1),
        }
    }

    fn init(
        &mut self,
        mut table: ActiveTopLevelTable,
        mut kernel_allocator: VirtualRegionAllocator,
    ) {
        extern "C" {
            pub static TDATA_START: u8;
            pub static TDATA_END: u8;

            pub static TBSS_START: u8;
            pub static TBSS_END: u8;
        }

        let tdata_size =
            unsafe { &TDATA_END as *const _ as usize - &TDATA_START as *const _ as usize };
        let tbss_size =
            unsafe { &TBSS_END as *const _ as usize - &TBSS_START as *const _ as usize };
        let tls_size = tdata_size + tbss_size;

        let mut kernel_tls = kernel_allocator
            .alloc(tls_size)
            .expect("Unable to allocate memory for TLS.");
        kernel_tls.map(&mut table, EntryFlags::WRITABLE | EntryFlags::NO_EXECUTE);

        let tbss_start = kernel_tls.start() + tdata_size;
        unsafe {
            ptr::copy(
                &TDATA_START as *const u8,
                kernel_tls.start().as_mut_ptr(),
                tbss_start - kernel_tls.start(),
            );
            ptr::write_bytes(tbss_start.as_mut_ptr(), 0, tbss_size);
            let fs_pointer = (*kernel_tls.end() as usize - size_of::<usize>()) as *mut usize;
            ptr::write(fs_pointer, *kernel_tls.end());
            register::write::fs_base(VirtualAddress::new(fs_pointer as usize));
        }

        self.tasks.insert(
            TaskID(0),
            Arc::new(Task {
                pager: Mutex::new(Pager::new(TopLevelTable::Active(table), kernel_allocator)),
                _kernel_tls: kernel_tls,
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

fn create_task_list() -> RwLock<TaskList> {
    RwLock::new(TaskList::new())
}

pub fn task_list() -> RwLockReadGuard<'static, TaskList> {
    TASK_LIST.call_once(create_task_list).read()
}

pub fn task_list_mut() -> RwLockWriteGuard<'static, TaskList> {
    TASK_LIST.call_once(create_task_list).write()
}

pub fn current_task() -> Arc<Task> {
    task_list().current().clone()
}

pub fn init(table: ActiveTopLevelTable, kernel_allocator: VirtualRegionAllocator) {
    task_list_mut().init(table, kernel_allocator);
}
