use super::interrupt;
use crate::memory::VirtualAddress;

#[naked]
pub unsafe fn enter_ring3(instruction_pointer: VirtualAddress, stack_pointer: VirtualAddress) -> ! {
    interrupt::disable_always();
    // TODO: Set up kernel and user gs for switching
    // TODO: Once we can actually do IPL interrupts, this should be 0x202
    llvm_asm!("mov r11, 0x2 
          sysretq"
         ::
         "{rcx}"(instruction_pointer)
         "{rsp}"(stack_pointer)
         ::
         "intel",
         "volatile");
    unreachable!()
}
