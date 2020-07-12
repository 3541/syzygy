mod list;
mod task;

use alloc::sync::Arc;
use core::sync::atomic::{AtomicBool, Ordering};

use logc::trace;

pub use list::{TaskId, TaskList};
pub use task::Task;

use crate::arch::{interrupt, pause};
use crate::memory::paging::table::ActiveTopLevelTable;
use crate::memory::region::VirtualRegionAllocator;

/// # Safety
/// Implementers must be inline(always)
pub unsafe trait CpuState {
    unsafe fn save(&mut self);
    unsafe fn restore(&self);
}

pub fn init(table: ActiveTopLevelTable, kernel_allocator: Arc<VirtualRegionAllocator>) {
    TaskList::the_mut().init(table, kernel_allocator);
}

#[thread_local]
static SWITCHING: AtomicBool = AtomicBool::new(false);

pub fn switch_to(target_task: TaskId) {
    trace!("About to switch tasks. Trying to get switch lock.");
    while SWITCHING.compare_and_swap(false, true, Ordering::SeqCst) {
        pause();
    }

    trace!("Acquiring pointers to tasks.");

    // Acquire pointers to the tasks so locks are not held into the switch.
    let (current, target) = {
        let list = TaskList::the();
        let mut current = list
            .get(TaskList::current_id())
            .expect("Current task does not exist.")
            .lock();
        let mut target = list
            .get(target_task)
            .expect("Target task does not exist.")
            .lock();

        (&mut *current as *mut Task, &mut *target as *mut Task)
    };

    TaskList::set_current_id(target_task);

    trace!(
        "About to switch. Pointers are 0x{:x} and 0x{:x}",
        current as usize,
        target as usize
    );

    trace!("Releasing switch lock.");

    interrupt::disable();
    SWITCHING.store(false, Ordering::SeqCst);
    unsafe { (*current).switch_to(&mut *target) };
    interrupt::enable();
}
