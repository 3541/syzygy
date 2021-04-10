# Useful utilities for VMM debugging.

# Get the index into the given page table for an address.
# level is 4 for PML4, 1 for PT.
def index_of(level, addr):
    return (addr >> (12 + 9 * (level - 1))) & ((1 << 9) - 1)

# Turn a table entry into an address.
def entry_to_addr(entry):
    return entry & 0x000ffffffffff000
