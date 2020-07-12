use alloc::sync::Arc;
use core::mem::size_of;

use logc::trace;
use spin::Mutex;

use super::{CpuState, TaskList};
use crate::arch::interrupt;
use crate::arch::task::State;
use crate::memory::paging::{EntryFlags, Pager, TopLevelTable};
use crate::{Address, VirtualAddress, VirtualRegion};

pub enum TaskState {
    Active,
    Inactive,
}

#[allow(dead_code)]
pub struct Task {
    pager: Pager,
    kernel_tls: VirtualRegion,
    stack_pointer: VirtualAddress,
    kernel_stack: VirtualRegion,
    state: TaskState,
    cpu_state: State,
}

impl Task {
    pub unsafe fn create_existing(
        pager: Pager,
        kernel_tls: VirtualRegion,
        kernel_stack: VirtualRegion,
    ) -> Task {
        Task {
            pager,
            kernel_tls,
            stack_pointer: VirtualAddress::new(0),
            state: TaskState::Active,
            kernel_stack,
            cpu_state: State::empty(),
        }
    }

    pub fn new(&mut self, f: fn()) -> Task {
        let current_pager = self.pager();
        let new_pager = current_pager.new();
        let new_kernel_allocator = new_pager.kernel_allocator();

        let mut kernel_stack = new_kernel_allocator
            .alloc(8192)
            .expect("Unable to allocate virtual memory for new task stack.");
        current_pager
            .active_table()
            .with(&mut new_pager.inactive_table(), |mapper| {
                kernel_stack.map(mapper, EntryFlags::WRITABLE | EntryFlags::NO_EXECUTE)
            });

        // We shouldn't have a per-task kernel virtual allocator.
        // This will be necessary when user stacks are introduced.

        /*        let mut current_mapper = current_pager.active_table();
        let mut current_kernel_allocator = current_pager.kernel_allocator();
        let mut temp_mapping = current_kernel_allocator.alloc(8192).expect(
            "Unable to allocated virtual memory for temporary mapping of new kernel stack.",
        );
        temp_mapping.map_to(
            &mut current_mapper,
            kernel_stack.backing().unwrap().clone(),
            EntryFlags::WRITABLE | EntryFlags::NO_EXECUTE,
        );*/

        trace!(
            "Writing function pointer 0x{:x} to 0x{:x}.",
            f as usize,
            unsafe { kernel_stack.end().as_mut_ptr::<usize>().offset(-1) as usize }
        );
        unsafe {
            kernel_stack
                .end()
                .as_mut_ptr::<usize>()
                .offset(-1)
                .write_volatile(f as usize)
        };

        let mut cpu_state = State::empty();
        cpu_state.set_fs_base(*(self.kernel_tls.end() - size_of::<usize>()) as u64);

        Task {
            pager: new_pager,
            stack_pointer: kernel_stack.end() - size_of::<usize>(),
            kernel_stack,
            kernel_tls: self.kernel_tls.clone(),
            state: TaskState::Inactive,
            cpu_state,
        }
    }

    pub fn pager(&self) -> &Pager {
        &self.pager
    }

    pub fn current() -> Arc<Mutex<Task>> {
        TaskList::the().current().clone()
    }

    #[naked]
    pub(super) unsafe fn switch_to(&mut self, other: &mut Task) {
        interrupt::disable();
        /*        asm!(
                // Preserve callee-saved registers.
                "push rbx", "push r12", "push r13", "push r14", "push r15", "push rbp"
        );*/
        self.cpu_state.save();

        self.state = TaskState::Inactive;
        other.state = TaskState::Active;

        // Switch to the new PML4
        {
            let new_table = other.pager.take_inactive_table();

            let old_table = self.pager.active_table().switch(new_table);

            let new_active_table = self.pager.take_active_table();
            *self.pager.page_table() = TopLevelTable::Inactive(Mutex::new(old_table));
            *other.pager.page_table() = TopLevelTable::Active(Mutex::new(new_active_table));
        }

        // Use llvm_asm because it can write to VirtualAddress.
        // Note that this is okay because VirtualAddress is repr(transparent).
        llvm_asm!("mov $0, rsp" : "=r"(self.stack_pointer) :: "memory" : "intel", "volatile");
        llvm_asm!("mov rsp, $0" :: "r"(other.stack_pointer) :: "intel", "volatile");

        /*        asm!(
                // Restore callee-saved registers.
                "pop rbp", "pop r15", "pop r14", "pop r13", "pop r12", "pop rbx"
        );*/
        other.cpu_state.restore();

        // And return into the new task.
    }
}
