ENTRY(_start)

KERNEL_BASE = 0xC0000000;

SECTIONS {
    . = 1M + KERNEL_BASE;

    .text ALIGN(4K) : AT(ADDR(.text) - KERNEL_BASE) {
        KEEP(*(.multiboot_header))
        *(.text .text.*)
    }

    .data ALIGN(4K) : AT(ADDR(.data) - KERNEL_BASE) {
        *(.data .data.*)
    }

    .bss ALIGN(4K) : AT(ADDR(.bss) - KERNEL_BASE) {
        *(.bss .bss.*)
    }

    .rodata ALIGN(4K) : AT(ADDR(.rodata) - KERNEL_BASE) {
        *(.rodata .rodata.*)
    }

    .data.rel.ro ALIGN(4K) : AT(ADDR(.data.rel.ro) - KERNEL_BASE) {
        *(.data.rel.ro.local*) *(.data.rel.ro .data.rel.ro.*)
    }
}
