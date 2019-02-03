%ifndef KERNEL_BASE
	%define KERNEL_BASE 0
%endif

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
	mov dword [0xb8000], 0x4F524F45 - KERNEL_BASE
	mov dword [0xb8004], 0x4F3A4F52 - KERNEL_BASE
	mov dword [0xb8008], 0x4F204F20 - KERNEL_BASE
	mov byte [0xb800a], al
	hlt
