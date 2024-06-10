ENTRY(kinit)

SZ_BASE = 0xFFFFFFFF80100000;

SECTIONS {
    . = SZ_BASE;

    .text ALIGN(4K) : {
        *(.text .text.*)
    }

    .data ALIGN(4K) : {
        *(.data .data.*)
    }

    .bss ALIGN(4K) : {
        *(.bss .bss.*)
    }

    .rodata ALIGN(4K) : {
        *(.rodata .rodata.*)
    }

    .tdata ALIGN(4K) : {
        *(.tdata .tdata.*)
    }

    .tbss ALIGN(4K) : {
        *(.tbss .tbss.*)
    }
}
