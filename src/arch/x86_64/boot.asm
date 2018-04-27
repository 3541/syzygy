extern lm_start

%include "src/arch/x86_common/multiboot_header.asm"
%include "src/arch/x86_common/boot.asm"

nc_start:
	call check_long_mode

	call setup_paging
	call enable_paging

	lgdt [gdt64.pointer]

	jmp gdt64.code:lm_start

check_long_mode:
	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000001
	jb .no_long_mode

	mov eax, 0x80000001
	cpuid
	test edx, 1 << 29
	jz .no_long_mode
	ret
.no_long_mode:
	mov al, "2"
	jmp error

setup_paging:
	mov eax, pdpt
	or eax, 0b11 ; present, writable
	mov [pml4], eax

	mov eax, pd
	or eax, 0b11 ; present, writable
	mov [pdpt], eax

	mov ecx, 0
.map_pd:
	mov eax, 0x200000 ; 2 MiB
	mul ecx
	or eax, 0b10000011 ; present, writable, huge (2 MiB)
	mov [pd + ecx * 8], eax

	inc ecx
	cmp ecx, 512
	jne .map_pd

	ret

enable_paging:
	mov eax, pml4
	mov cr3, eax

	; set PAE flag
	mov eax, cr4
	or eax, 1 << 5
	mov cr4, eax

	; set long mode in EFER MSR
	mov ecx, 0xC0000080
	rdmsr
	or eax, 1 << 8
	wrmsr

	; enable paging
	mov eax, cr0
	or eax, 1 << 31
	mov cr0, eax

	ret

section .rodata
gdt64:
	dq 0
.code: equ $ - gdt64
	dq (1 << 43) | (1 << 44) | (1 << 47) | (1 << 53) ; code segment
.pointer:
	dw $ - gdt64 - 1
	dq gdt64

section .bss
align 4096
pml4:
	resb 4096
pdpt:
	resb 4096
pd:
	resb 4096
%include "src/arch/x86_common/stack.asm"
