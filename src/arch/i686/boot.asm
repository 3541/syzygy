%include "src/arch/x86_common/header.asm"

KERNEL_BASE equ 0xC0000000
KERNEL_PDP_INDEX equ 3
KERNEL_PD_INDEX equ 0

global _start
section .text
bits 32
_start:
	mov esp, stack_top - KERNEL_BASE

	call check_multiboot
	call check_cpuid
	call check_pae
	
	call setup_page_tables
	call enable_paging

	lea ecx, [higher_half]
	jmp ecx
	
	hlt

setup_page_tables:
	mov eax, pd - KERNEL_BASE
	or eax, 0b11 ; Present and writable
	mov [pdp - KERNEL_BASE], eax
	mov [pdp + KERNEL_PDP_INDEX * 8 - KERNEL_BASE], eax

;	mov eax, 0x200000
	mov eax, 0
	or eax, 0b10000011
	mov [pd + KERNEL_PD_INDEX - KERNEL_BASE], eax

	ret

enable_paging:
	mov eax, pdp - KERNEL_BASE
	mov cr3, eax

	mov eax, cr4
	or eax, 1 << 5 ; Enable PAE
	or eax, 1 << 4 ; Enable PSE
	mov cr4, eax

	mov eax, cr0
	or eax, 1 << 31
	mov cr0, eax
	hlt
	ret


higher_half:
	mov dword [0xC00B8000], 0x2F4B2F4F

	hlt

%include "src/arch/x86_common/util.asm"

section .bss
align 0x1000
pdp:
	resb 32
pd:
	resb 0x1000	
