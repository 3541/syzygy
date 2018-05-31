ENTRY(_start)

KERNEL_BASE = 0xC0000000;

SECTIONS {
    . = KERNEL_BASE + 0x100000;

    .text ALIGN(4K) : AT(ADDR(.text) - KERNEL_BASE) {
        KEEP(*(.multiboot_header))
        *(.text .text.*)
    }

    .bss : AT(ADDR(.bss) - KERNEL_BASE) {
        *(.bss)
    }

    .rodata : {
        *(.rodata .rodata.*)
    }

    .data.rel.ro : {
        *(.data.rel.ro.local*) *(.data.rel.ro .data.rel.ro.*)
    }
}
