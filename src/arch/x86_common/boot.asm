%include "./multiboot_header.asm"

section .bss
align 16
stack_bottom:
	resb 16384
stack_top:

section .text
bits 32
global _start
_start:
	mov esp, stack_top

	mov dword [0xb8000], 0x2F4F2F4B
	hlt
