global lm_start

section .text
bits 64
lm_start:
	mov rax, 0x2F4B2F4F2F4D2F4C ; LMOK
	mov qword [0xb8000], rax
	hlt
