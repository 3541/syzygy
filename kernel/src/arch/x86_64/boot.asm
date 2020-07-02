%define KERNEL_BASE 0xFFFFC00000000000
%define KERNEL_PML4_INDEX 384
%define KERNEL_PDP_INDEX 0

global _start
section .inittext
bits 32
_start:
	mov esp, stack_top - KERNEL_BASE
	; Pass multiboot info struct
	mov edi, ebx

	call check_multiboot
	call check_cpuid
	call check_pae

	call setup_page_tables
	call enable_paging

	mov dword [0xb8000], 0x2F472F50
	mov dword [0xb8004], 0x2F4B2F4F

	lgdt [gdt64.pointer_low - KERNEL_BASE]

	mov ax, gdt64.data
	mov ss, ax
	mov ds, ax
	mov es, ax

	jmp gdt64.code:preinit64

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

check_pae:
	mov eax, 1
	cpuid

	test edx, 1 << 6
	jz .no
	ret
.no:
	mov al, '2'
	jmp err

err:
	mov dword [0xb8000], 0x4F524F45
	mov dword [0xb8004], 0x4F3A4F52
	mov dword [0xb8008], 0x4F204F20
	mov byte [0xb800a], al
	hlt


check_long_mode:

	; check for extended info
	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000001
	jb .no_long_mode

	; check for long mode
	mov eax, 0x80000001
	cpuid
	test edx, 1 << 29
	jz .no_long_mode
	ret
.no_long_mode:
	mov al, '2'
	jmp err

setup_page_tables:
	; Map pml4 recursively
	mov eax, pml4 - KERNEL_BASE
	or eax, 0b11
	mov [pml4 - KERNEL_BASE + 4088], eax

	; Map PDP identity and higher half
	mov eax, pdp - KERNEL_BASE
	or eax, 0b11
	mov [pml4 - KERNEL_BASE], eax
	mov [pml4 - KERNEL_BASE + KERNEL_PML4_INDEX * 8], eax

	mov eax, 0
	or eax, 0b10000011
	mov [pdp - KERNEL_BASE], eax

	ret

enable_paging:
	; Load PML4 to CR3
	mov eax, pml4 - KERNEL_BASE
	mov cr3, eax

	; Enable PAE
	mov eax, cr4
	or eax, 1 << 5
	mov cr4, eax

	mov ecx, 0xC0000080
	rdmsr
	; Enable long mode
	or eax, 1 << 8
	wrmsr

	; Enable paging
	mov eax, cr0
	or eax, 1 << 31
	mov cr0, eax

	ret

bits 64
preinit64:
	mov rax, init64
	jmp rax

section .text
init64:
	; Unfuck stack, load higher-half address for GDT
	mov rsp, stack_top
	mov rax, gdt64.pointer
	lgdt [rax]

	mov ax, gdt64.data
	mov ss, ax
	mov ds, ax
	mov es, ax


	; Unmap identity mapping
	mov rax, pml4
	mov qword [rax], 0
	invlpg [0]

	mov rax, 0x2F4B2F4F2F342F36
        mov rcx, 0xFFFFC000000B8000
        mov qword [rcx], rax

	jmp start64

start64:
        ;; rdmsr/wrmsr act on ecx and edx:eax
        mov ecx, 0xC0000080
        rdmsr
        ;; Enable NX
        or eax, 1 << 11
        ;; Enable syscall/sysret
        or eax, 1    
        wrmsr

        mov ecx, 0xC0000081
        rdmsr
        ;; This insanity is because sysret always sets the CS selector to
        ;; STAR.SYSRET_CS + 16 and SS to + 8.
        or edx, gdt64.data << 16
        ;; Set RPL to 3
        or edx, 3 << 16
        ;; For syscall
        or edx, gdt64.code
        wrmsr
        

        ;; Enable WP
        mov rax, cr0
        or rax, 1 << 16
        mov cr0, rax

        ;; pass stack_bottom
        mov rsi, stack_bottom

        ;; make rbp null so that there is an endpoint for stack walking
        mov rbp, 0

	extern kmain
	call kmain

	mov rax, 0xFF202F542F452F52
	mov rcx, 0xFFFFC000000B8000
	mov qword [rcx], rax

	hlt

section .bss
pml4:
	resb 0x1000
pdp:
	resb 0x1000

section .rodata
gdt64:
	dq 0
.code: equ $ - gdt64
        ;; executable  type        present     x86_64 code
	dq (1 << 43) | (1 << 44) | (1 << 47) | (1 << 53)
.data: equ $ - gdt64
        ;; writable
	dq (1 << 41) | (1 << 44) | (1 << 47)
;;.userdata: equ $ - gdt64       
;;       dq (1 << 41) | (1 << 44) | (1 << 45) | (1 << 46) | (1 << 47)
;; .usercode: equ $ - gdt64
        ;;                         user        mode
;;         dq (1 << 43) | (1 << 44) | (1 << 45) | (1 << 46) | (1 << 47) | (1 << 53)

.end:
.pointer:
	dw gdt64.end - gdt64 -1
	dq gdt64
.pointer_low:
	dw gdt64.end - gdt64 -1
	dq gdt64 - KERNEL_BASE

section .bss
align 0x1000
stack_bottom:
	resb 32768
stack_top:
