global start

section .text
bits 32
start:
	mov esp, stack_bottom
	mov edi, ebx
	call check_multiboot
	call check_cpuid
	call check_long_mode
	mov dword [0xb8000], 0x2F4B2F4F
	jmp nc_start

check_multiboot:
	cmp eax, 0x36D76289
	jne .no_multiboot
	ret
.no_multiboot:
	mov al, "0"
	jmp error

check_cpuid:
	pushfd
	pop eax

	; backup FLAGS
	mov ecx, eax

	xor eax, 1 << 21

	push eax
	popfd

	pushfd
	pop eax

	; restore FLAGS
	push ecx
	popfd

	cmp eax, ecx
	je .no_cpuid
	ret
.no_cpuid:
	mov al, "1"
	jmp error

error:
	mov dword [0xb8000], 0x4F524F45
	mov dword [0xb8004], 0x4F3A4F52
	mov dword [0xb8008], 0x4F204F20
	mov byte [0xb800a], al
	hlt


