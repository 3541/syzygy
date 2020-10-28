ENTRY(kinit)

SZ_KERNEL_BASE = 0xFFFFFFFF80000000;

SECTIONS {
         . = SZ_KERNEL_BASE + 0x100000;

         .stivale2hdr ALIGN(64K) : {
              KEEP(*(.stivale2hdr))
         }

         .text ALIGN(64K) : {
              *(.text .text.*)
         }

         .data ALIGN(64K) : {
              *(.data .data.*)
         }

         .bss ALIGN(64K) : {
              *(.bss .bss.*)
         }

         .rodata ALIGN(64K) : {
              *(.rodata .rodata.*)
         }
}
