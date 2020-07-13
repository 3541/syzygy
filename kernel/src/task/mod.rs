mod scheduler;
mod task;

use alloc::sync::Arc;

pub use scheduler::{Scheduler, TaskId};
pub use task::Task;

use crate::memory::paging::table::ActiveTopLevelTable;
use crate::memory::region::VirtualRegionAllocator;

/// # Safety
/// Implementers must be inline(always)
pub unsafe trait CpuState {
    unsafe fn save(&mut self);
    unsafe fn restore(&self);
}

pub fn init(table: ActiveTopLevelTable, kernel_allocator: Arc<VirtualRegionAllocator>) {
    Scheduler::the_mut().init(table, kernel_allocator);
}
