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

	call check_multiboot
	call check_cpuid

	mov dword [0xb8000], 0x2F4B2F4F

	jmp arch_start

	hlt

check_multiboot:
	cmp eax, 0x36D76289
	jne .no
	ret
.no:
	mov al, '0'
	jmp err

check_cpuid:
	pushfd
	pushfd

	xor dword [esp], 1 << 21
	popfd

	pushfd
	pop eax

	mov dword ecx, [esp]

	popfd

	cmp eax, ecx
	je .no
	ret
.no:
	mov al, '1'
	jmp err

err:
	mov dword [0xb8000], 0x4F524F45
	mov dword [0xb8004], 0x4F3A4F52
	mov dword [0xb8008], 0x4F204F20
	mov byte [0xb800a], al
	hlt
