%include "src/arch/x86_common/header.asm"

global _start
section .text
bits 32
_start:
	mov esp, stack_top

	call check_multiboot
	call check_cpuid
	call check_pae

	mov dword [0xb8000], 0x2F4B2F4F

	hlt

%include "src/arch/x86_common/util.asm"
