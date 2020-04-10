use crate::memory::VirtualAddress;

#[naked]
pub unsafe fn enter_ring3(instruction_pointer: VirtualAddress, stack_pointer: VirtualAddress) -> ! {
    asm!("mov r11, 0x202
          sysretq"
         ::
         "{rcx}"(instruction_pointer)
         "{rsp}"(stack_pointer)
         ::
         "volatile");
    unreachable!()
}
