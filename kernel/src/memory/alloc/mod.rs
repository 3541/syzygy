mod bump_allocator;
//mod fake_allocator;

use super::heap::{HEAP_BASE, HEAP_SIZE};
use crate::sync::SpinLocked;
use bump_allocator::BumpAllocator;
//use fake_allocator::FakeAllocator;

//#[global_allocator]
//static ALLOCATOR: FakeAllocator = FakeAllocator {};

#[cfg(not(test))]
#[global_allocator]
static ALLOCATOR: SpinLocked<BumpAllocator> =
    unsafe { SpinLocked::new(BumpAllocator::new(HEAP_BASE, HEAP_SIZE)) };
