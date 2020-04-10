        global _start
        section .text
_start:
        ud2
        jmp _start
