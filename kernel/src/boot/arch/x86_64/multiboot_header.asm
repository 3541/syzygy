        section .multiboot_header
header_start:
        dd 0xE85250D6           ; Magic
        dd 0                    ; i386
        dd header_end - header_start ; Length
        dd 0x100000000 - (0xE85250D6 + (header_end - header_start)) ; Checksum

        align 8

        ;; End tag
        dw 0
        dw 0
        dd 8
header_end:
