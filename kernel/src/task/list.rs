use alloc::sync::Arc;
use core::mem::size_of;
use core::ptr;
use core::sync::atomic::{AtomicUsize, Ordering};

use hashbrown::HashMap;
use spin::{Mutex, Once, RwLock, RwLockReadGuard, RwLockWriteGuard};

use super::Task;
use crate::arch::register;
use crate::memory::paging::{ActiveTopLevelTable, EntryFlags, Pager, TopLevelTable};
use crate::memory::region::VirtualRegionAllocator;
use crate::memory::{Address, VirtualAddress, VirtualRegion};

static TASK_LIST: Once<RwLock<TaskList>> = Once::new();

// TODO: Static Arc<Mutex>> of the current task?
#[thread_local]
static TASK_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Eq, Hash, PartialEq, Debug, Copy, Clone)]
pub struct TaskId(pub usize);

impl TaskId {
    fn next(&mut self) -> TaskId {
        let ret = TaskId(self.0);
        self.0 += 1;
        ret
    }
}

pub struct TaskList {
    tasks: HashMap<TaskId, Arc<Mutex<Task>>>,
    next_id: TaskId,
}

impl TaskList {
    fn new() -> RwLock<TaskList> {
        RwLock::new(TaskList {
            tasks: HashMap::new(),
            next_id: TaskId(1),
        })
    }

    pub(super) fn init(
        &mut self,
        mut table: ActiveTopLevelTable,
        kernel_allocator: Arc<VirtualRegionAllocator>,
    ) {
        extern "C" {
            pub static TDATA_START: u8;
            pub static TDATA_END: u8;

            pub static TBSS_START: u8;
            pub static TBSS_END: u8;

            pub static INIT_STACK_START: u8;
            pub static INIT_STACK_END: u8;
        }

        let tdata_size =
            unsafe { &TDATA_END as *const _ as usize - &TDATA_START as *const _ as usize };
        let tbss_size =
            unsafe { &TBSS_END as *const _ as usize - &TBSS_START as *const _ as usize };
        let tls_size = tdata_size + tbss_size;

        let mut kernel_tls = kernel_allocator
            .alloc(tls_size)
            .expect("Unable to allocate memory for TLS.");
        kernel_tls.map(
            &mut table,
            EntryFlags::WRITABLE | EntryFlags::NO_EXECUTE | EntryFlags::GLOBAL,
        );

        let mut tbss_start = kernel_tls.start() + tdata_size;
        unsafe {
            ptr::copy_nonoverlapping(
                &TDATA_START as *const u8,
                kernel_tls.start().as_mut_ptr(),
                tbss_start - kernel_tls.start(),
            );
            ptr::write_bytes(tbss_start.as_mut_ptr::<u8>(), 0, tbss_size);
            let fs_pointer = (*kernel_tls.end() as usize - size_of::<usize>()) as *mut usize;
            ptr::write(fs_pointer, *kernel_tls.end());
            register::write::fs_base(VirtualAddress::new(fs_pointer as usize));
        }

        let stack_start = unsafe { VirtualAddress::new(&INIT_STACK_START as *const u8 as usize) };
        let stack_end = unsafe { VirtualAddress::new(&INIT_STACK_END as *const u8 as usize) };

        self.tasks.insert(
            TaskId(0),
            Arc::new(Mutex::new(unsafe {
                Task::create_existing(
                    Pager::create_existing(
                        TopLevelTable::Active(Mutex::new(table)),
                        kernel_allocator,
                    ),
                    kernel_tls,
                    VirtualRegion::new(stack_start, stack_end - stack_start),
                )
            })),
        );
    }

    pub fn spawn(&mut self, f: fn()) {
        let new_task = self.current().lock().new(f);
        self.tasks
            .insert(self.next_id.next(), Arc::new(Mutex::new(new_task)));
    }

    pub fn the() -> RwLockReadGuard<'static, TaskList> {
        TASK_LIST.call_once(TaskList::new).read()
    }

    pub fn the_mut() -> RwLockWriteGuard<'static, TaskList> {
        TASK_LIST.call_once(TaskList::new).write()
    }

    pub fn current_id() -> TaskId {
        TaskId(TASK_ID.load(Ordering::SeqCst))
    }

    pub(super) fn set_current_id(id: TaskId) {
        TASK_ID.store(id.0, Ordering::SeqCst)
    }

    pub fn current(&self) -> &Arc<Mutex<Task>> {
        &self
            .tasks
            .get(&TaskList::current_id())
            .expect("Current task is missing.")
    }

    pub fn get(&self, id: TaskId) -> Option<&Arc<Mutex<Task>>> {
        self.tasks.get(&id)
    }
}
