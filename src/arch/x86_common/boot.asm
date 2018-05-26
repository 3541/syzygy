%include "src/arch/x86_common/multiboot_header.asm"

section .bss
align 16
stack_bottom:
	resb 16384
stack_top:


global _start
section .text
bits 32
_start:
	mov esp, stack_top

	mov dword [0xb8000], 0x2F4B2F4F

	mov al, 'a'
	jmp err
	hlt

err:
	mov dword [0xb8000], 0x4F524F45
	mov byte [0xb8005], 0x4F
	mov byte [0xb8004], al
	hlt
