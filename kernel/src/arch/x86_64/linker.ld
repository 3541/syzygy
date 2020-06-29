ENTRY(_start)

KERNEL_BASE = 0xFFFFC00000000000;

SECTIONS {
    . = 1M;
    . += SIZEOF_HEADERS;

    .inittext : {
        *(.inittext)
    }

    . = KERNEL_BASE;
    . += 1M;
    . += SIZEOF(.inittext);
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

    .tdata ALIGN(4K) : AT(ADDR(.tdata) - KERNEL_BASE) {
        TDATA_START = .;
        *(.tdata .tdata.*)
        . += 8;
        . = ALIGN(4K);
        TDATA_END = .;
    }

    .tbss ALIGN(4K) : AT(ADDR(.tbss) - KERNEL_BASE) {
        TBSS_START = .;
        *(.tbss .tbss.*)
        . = ALIGN(4K);
        TBSS_END = .;
    }

/*    .data.rel.ro ALIGN(4K) : AT(ADDR(.data.rel.ro) - KERNEL_BASE) {
        *(.data.rel.ro.local*) *(.data.rel.ro .data.rel.ro.*)
        . = ALIGN(4K);
    }*/
}
