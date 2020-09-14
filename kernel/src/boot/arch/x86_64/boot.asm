        ;; This must be defined, instead of using the linker script symbol,
	;; because Nasm refuses to do arithmetic on two labels at assembly time.
        %define KERNEL_BASE 0xFFFFC00000000000
        %define KERNEL_PML4_INDEX 384

        %define PAGE_SIZE 0x200000 ; Kernel is initially mapped with 2 MiB pages.
        %define PAGE_WRITABLE 0b11
        %define PAGE_SIZE_BIT (1 << 7)

        %define MSR_EFER 0xC0000080

        global _start
        section .init
        bits 32
_start:
        extern check_multiboot
        extern check_cpuid
        extern check_pae
        extern check_long_mode

        mov esp, SZ_INIT_STACK_TOP - KERNEL_BASE
        mov edi, ebx            ; Hold on to Multiboot info.

        call check_multiboot
        call check_cpuid
        call check_pae
        call check_long_mode

        call paging_init
        call paging_enable

	mov dword [0xb8000], 0x2F472F50
	mov dword [0xb8004], 0x2F4B2F4F

        ;; Load GDT.
        lgdt [SZ_INIT_GDTR - KERNEL_BASE]

        mov ax, SZ_INIT_GDT.data
        mov ss, ax
        mov ds, ax
        mov es, ax

        jmp SZ_INIT_GDT.code:preinit64

        hlt

paging_init:
        ;; Map PML4 recursively.
        mov eax, SZ_INIT_PML4 - KERNEL_BASE
        or eax, PAGE_WRITABLE
        mov [SZ_INIT_PML4 - KERNEL_BASE + 511 * 8], eax

        ;; Map the PDP higher-half in the PML4.
        mov eax, SZ_INIT_PDP - KERNEL_BASE
        or eax, PAGE_WRITABLE
        mov [SZ_INIT_PML4 - KERNEL_BASE], eax
        mov [SZ_INIT_PML4 - KERNEL_BASE + KERNEL_PML4_INDEX * 8], eax

        ;; Map the PD in the PDP.
        mov eax, SZ_INIT_PD - KERNEL_BASE
        or eax, PAGE_WRITABLE
        mov [SZ_INIT_PDP - KERNEL_BASE], eax

        ;; Map a 2 MiB page in the PD.
        mov ecx, 0
        .map_pd:
        mov eax, PAGE_SIZE
        mul ecx
        or eax, PAGE_WRITABLE | PAGE_SIZE_BIT
        mov [SZ_INIT_PD - KERNEL_BASE + ecx * 8], eax

        inc ecx
        cmp ecx, 512
        jl .map_pd

        ret

paging_enable:
        ;; Load CR3.
        mov eax, SZ_INIT_PML4 - KERNEL_BASE
        mov cr3, eax

        ;; Enable PAE.
        mov eax, cr4
        or eax, 1 << 5
        mov cr4, eax

        ;; Enable long mode.
        mov ecx, MSR_EFER
        rdmsr
        or eax, 1 << 8
        wrmsr

        ;; Enable paging.
        mov eax, cr0
        or eax, 1 << 31         ; Paging.
        or eax, 1 << 16         ; WP.
        mov cr0, eax

        ret


        bits 64
preinit64:
        mov rax, init64
        jmp rax

        section .text
init64:
        mov rsp, SZ_INIT_STACK_TOP

        mov rax, 0x2F4B2F4F2F342F36
        mov rcx, 0xFFFFC000000B8000
        mov qword [rcx], rax

        ;; Enable NX and SYSCALL.
        mov ecx, MSR_EFER
        rdmsr
        or eax, 1 << 11         ; NX
        or eax, 1               ; SYSCALL
        wrmsr

        ;; Make a detectable termination point for stack walking.
        mov rbp, 0

        extern kmain
        call kmain

        mov rax, 0xFF202F542F452F52
	mov rcx, 0xFFFFC000000B8000
	mov qword [rcx], rax

        hlt

        section .rodata
        global SZ_INIT_GDT
        global SZ_INIT_GTDR

SZ_INIT_GDT:
        dq 0                    ; Null selector.
        ;; writable    executable        type     present  64bit code
       .code: equ $ - SZ_INIT_GDT
        ;;                   true        code        true        true
        dq              (1 << 43) | (1 << 44) | (1 << 47) | (1 << 53)
        .data: equ $ - SZ_INIT_GDT
        dq  (1 << 41)             | (1 << 44) | (1 << 47)
        .end:

SZ_INIT_GDTR:
        dw SZ_INIT_GDT.end - SZ_INIT_GDT - 1
        dq SZ_INIT_GDT
        
        section .bss
        align 0x1000

        global SZ_INIT_PML4
        global SZ_INIT_PDP
        global SZ_INIT_PD
        global SZ_INIT_STACK_BOTTOM
        global SZ_INIT_STACK_TOP

SZ_INIT_PML4:
        resb 0x1000
SZ_INIT_PDP:
        resb 0x1000
SZ_INIT_PD:
        resb 0x1000
        
SZ_INIT_STACK_BOTTOM:
        resb 32768
SZ_INIT_STACK_TOP:
