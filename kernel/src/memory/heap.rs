use logc::trace;

use super::paging::mapper::Mapper;
use super::paging::table::EntryFlags;
use super::{Address, VirtualAddress, PAGE_SIZE};

pub const HEAP_BASE: VirtualAddress = unsafe { VirtualAddress::new_const(0x0000_4EA6_0000_0000) };
pub const HEAP_SIZE: usize = 64 * 1024; // 64 KiB

pub fn init_heap(mapper: &mut Mapper) {
    trace!(
        "Mapping heap from {} to {} (0x{:x})",
        HEAP_BASE,
        HEAP_BASE + HEAP_SIZE,
        HEAP_SIZE
    );
    for address in (*HEAP_BASE..(*HEAP_BASE + HEAP_SIZE)).step_by(PAGE_SIZE) {
        mapper.map(
            VirtualAddress::new(address),
            EntryFlags::PRESENT | EntryFlags::WRITABLE,
        );
    }
}
