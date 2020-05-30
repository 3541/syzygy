use logc::trace;

use super::paging::mapper::Mapper;
use super::paging::table::EntryFlags;
use super::size::MB;
use super::{Address, Frame, VirtualAddress};

pub const HEAP_BASE: VirtualAddress = unsafe { VirtualAddress::new_const(0x0000_4EA6_0000_0000) };
pub const HEAP_SIZE: usize = 4 * MB;

pub fn init_heap(mapper: &mut Mapper) {
    trace!(
        "Mapping heap from {} to {} (0x{:x})",
        HEAP_BASE,
        HEAP_BASE + HEAP_SIZE,
        HEAP_SIZE
    );
    for address in (*HEAP_BASE..(*HEAP_BASE + HEAP_SIZE)).step_by(Frame::SIZE) {
        mapper.map(
            VirtualAddress::new(address),
            EntryFlags::PRESENT | EntryFlags::WRITABLE,
        );
    }

    // Notify the bump allocator that the real heap is now available.
    unsafe { super::add_heap() };
}
