ENTRY(kinit)

SZ_KERNEL_BASE = 0xFFFFFFFF80100000;

SECTIONS {
         . = SZ_KERNEL_BASE;

         .stivale2hdr ALIGN(4K) : {
              KEEP(*(.stivale2hdr))
         }

         .text ALIGN(4K) : {
              SZ_KERNEL_TEXT_BASE = .;
              *(.text .text.*)
              SZ_KERNEL_TEXT_END = .;
         }

         .data ALIGN(4K) : {
              SZ_KERNEL_DATA_BASE = .;
              *(.data .data.*)
              SZ_KERNEL_DATA_END = .;
         }

         .bss ALIGN(4K) : {
              SZ_KERNEL_BSS_BASE = .;
              *(.bss .bss.*)
              SZ_KERNEL_BSS_END = .;
         }

         .rodata ALIGN(4K) : {
              SZ_KERNEL_RODATA_BASE = .;
              *(.rodata .rodata.*)
              SZ_KERNEL_RODATA_END = .;
         }

         .tdata ALIGN(4K) : {
              SZ_KERNEL_TDATA_BASE = .;
              *(.tdata .tdata.*)
              SZ_KERNEL_TDATA_END = .;
         }

         .tbss ALIGN(4K) : {
             SZ_KERNEL_TBSS_BASE = .;
             *(.tbss .tbss.*)
             SZ_KERNEL_TBSS_END = .;
         }
}
