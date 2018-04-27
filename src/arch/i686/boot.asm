%include "src/arch/x86_common/multiboot_header.asm"
%include "src/arch/x86_common/boot.asm"

nc_start:
	call setup_paging
	call enable_paging

	mov dword [0xb8000], 0xF24BF24F
	hlt

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
	mov eax, pd
	or eax,  0b11 ; present, writable
	mov [pdpt], eax

	mov eax, pt
	or eax, 0b11 ; present, writable
	mov [pd], eax
	
	mov ecx, 0
.map_pd:
	mov eax, 0x200000 ; 2 MiB
	mul ecx
	or eax, 0b10000011 ; present, writable, 2 MiB
	mov [pt + ecx * 8], eax

	inc ecx
	cmp ecx, 512
	jne .map_pd

	ret

enable_paging:
	mov eax, pdpt
	mov cr3, eax

	; enable PAE and PSE
	mov eax, cr4
	or eax, (1 << 1) | (1 << 5)
	mov cr4, eax

	; enable paging
	mov eax, cr0
	or eax, 1 << 31
	mov cr0, eax

	ret

section .bss
align 4096
pdpt:
	resb 32
pd:
	resb 4096
pt:
	resb 4096
%include "src/arch/x86_common/stack.asm"
