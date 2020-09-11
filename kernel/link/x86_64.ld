ENTRY(_start)

SZ_KERNEL_BASE = 0xFFFFC00000000000;

SECTIONS {
         . = 1M;
         . += SIZEOF_HEADERS;

         .init : {
               *(.init)
               . = ALIGN(4K);
         }

         . = SZ_KERNEL_BASE;
         . += 1M;
         . += SIZEOF(.init);
         . = ALIGN(4K);

         .text ALIGN(4K) : AT(ADDR(.text) - SZ_KERNEL_BASE) {
               KEEP(*(.multiboot_header))
               *(.text .text.*)
               . = ALIGN(4K);
         }

         .data ALIGN(4K) : AT(ADDR(.data) - SZ_KERNEL_BASE) {
               *(.data .data.*)
               . = ALIGN(4K);
         }

         .bss ALIGN(4K) : AT(ADDR(.bss) - SZ_KERNEL_BASE) {
               *(.bss .bss.*)
               . = ALIGN(4K);
         }

         .rodata ALIGN(4K) : AT(ADDR(.rodata) - SZ_KERNEL_BASE) {
               *(.rodata .rodata.*)
               . = ALIGN(4K);
         }
}
