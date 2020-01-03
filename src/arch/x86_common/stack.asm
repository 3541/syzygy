section .bss
align 0x1000
guard_page:
    resb 0x1000
stack_bottom:
	resb 16384
stack_top:
