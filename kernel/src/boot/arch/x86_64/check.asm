        section .init
        bits 32

        global check_multiboot
        global check_cpuid
        global check_pae
        global check_long_mode

err:
	mov dword [0xb8000], 0x4F524F45
	mov dword [0xb8004], 0x4F3A4F52
	mov dword [0xb8008], 0x4F204F20
	mov byte [0xb800a], al
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

check_pae:
        mov eax, 1
        cpuid

        test edx, 1 << 6
        jz .no
        ret

        .no:
        mov al, '2'
        jmp err
        
check_long_mode:
        mov eax, 0x80000000     ; Extended info support?
        cpuid
        cmp eax, 0x80000001
        jb .no

        mov eax, 0x80000001
        cpuid
        test edx, 1 << 29       ; Long mode?
        jz .no
        ret

        .no:
        mov al, '3'
        jmp err
