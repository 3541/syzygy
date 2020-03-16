section .multiboot_header
header_start:
	dd 0xE85250D6 ; multiboot2 magic number
	dd 0		  ; i386
	dd header_end - header_start ; length
	dd 0x100000000 - (0xE85250D6 + 0 + (header_end - header_start)) ; checksum

;align 8
    
;    dw 4    ; flags tag
;    dw 0    ; flags
;    dd 12   ; size
;    dd 1    ; text mode

;align 8

;    dw 5    ; framebuffer tag
;    dw 0    ; flags
;    dd 20   ; size
;    dd 80   ; width
;    dd 43   ; height
;    dd 0    ; depth (text mode)

align 8

	; end tag
	dw 0
	dw 0
	dd 8
header_end:
