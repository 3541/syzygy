%include "src/arch/x86_common/header.asm"

;KERNEL_BASE equ 0xC0000000
;KERNEL_PD_INDEX equ 768

%define KERNEL_BASE  0xC0000000
%define KERNEL_PD_INDEX 768

global _start
section .text
bits 32
_start:
	mov esp, stack_top - KERNEL_BASE

	call check_multiboot
	call check_cpuid
;	call check_pae
	
	call setup_page_tables
	call enable_paging

	mov dword [0xb8000], 0x2F472F50
	mov dword [0xb8004], 0x2F4B2F4F

	mov ecx, higher_half
	jmp ecx
	
	hlt

setup_page_tables:
	mov eax, 0
	; PS, present, writable
	or eax, 0b10000011

	; IDENTITY MAP
	mov [pd - KERNEL_BASE], eax
	; HIGHER HALF
	mov [pd - KERNEL_BASE + KERNEL_PD_INDEX * 4], eax

	ret

enable_paging:
	; Load page address
	mov eax, pd - KERNEL_BASE
	mov cr3, eax

	; Enable PAE
;	mov eax, cr4
;	or eax, 1 << 5
;	mov cr4, eax

	; Enable PSE
	mov eax, cr4
	or eax, 1 << 4
	mov cr4, eax

	; Enable paging
	mov eax, cr0
	or eax, 1 << 31
	or eax, 1
	mov cr0, eax

	ret


higher_half:
	; Unmap identity mapping
	mov dword [pd], 0
	invlpg [0]


	; Unfuck stack
	mov esp, stack_top

	mov dword [0xC00B8000], 0x2F4B2F4F ; OK

	; Pass multiboot info struct
	push ebx
	extern kmain
	call kmain

	; We probably shouldn't ever be here.
	mov dword [0xC00B8000], 0x2F452F52 ; RE
	mov word [0xC00B8004], 0x2F54 ; T

	hlt

%include "src/arch/x86_common/util.asm"

section .bss
align 0x1000
pd:
	resb 0x1000
pt:
	resb 0x1000
