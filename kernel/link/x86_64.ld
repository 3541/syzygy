ENTRY(kinit)

SZ_KERNEL_BASE = 0xFFFFFFFF80000000;

SECTIONS {
         . = SZ_KERNEL_BASE + 0x100000;

         .stivale2hdr ALIGN(4K) : {
              KEEP(*(.stivale2hdr))
         }

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
}
