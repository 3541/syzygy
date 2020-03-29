ENTRY(_start)

KERNEL_BASE = 0xFFFFC00000000000;

SECTIONS {
    . = 1M;

    .inittext : {
        *(.inittext)
    }

    . = KERNEL_BASE;
    . += SIZEOF_HEADERS;
    . = ALIGN(4K);

    .text ALIGN(4K) : AT(ADDR(.text) - KERNEL_BASE) {
        KEEP(*(.multiboot_header))
        *(.text .text.*)
        . = ALIGN(4K);
    }

    .data ALIGN(4K) : AT(ADDR(.data) - KERNEL_BASE) {
        *(.data .data.*)
        . = ALIGN(4K);
    }

    .bss ALIGN(4K) : AT (ADDR(.bss) - KERNEL_BASE) {
        *(.bss .bss.*)
        . = ALIGN(4K);
    }

    .rodata ALIGN(4K) : AT(ADDR(.rodata) - KERNEL_BASE) {
        *(.rodata .rodata.*)
        . = ALIGN(4K);
    }

    .data.rel.ro ALIGN(4K) : AT(ADDR(.data.rel.ro) - KERNEL_BASE) {
        *(.data.rel.ro.local*) *(.data.rel.ro .data.rel.ro.*)
        . = ALIGN(4K);
    }
}
