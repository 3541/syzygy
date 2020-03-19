use spin::{Mutex, MutexGuard};

use super::heap::{HEAP_BASE, HEAP_SIZE};

mod bump_allocator;
//mod fake_allocator;

use bump_allocator::BumpAllocator;
//use fake_allocator::FakeAllocator;

//#[global_allocator]
//static ALLOCATOR: FakeAllocator = FakeAllocator {};

#[global_allocator]
static ALLOCATOR: MutexWrapper<BumpAllocator> =
    unsafe { MutexWrapper::new(BumpAllocator::new(HEAP_BASE, HEAP_SIZE)) };

pub struct MutexWrapper<A>(Mutex<A>);

impl<A> MutexWrapper<A> {
    pub const fn new(allocator: A) -> Self {
        MutexWrapper(Mutex::new(allocator))
    }

    pub fn lock(&self) -> MutexGuard<A> {
        self.0.lock()
    }
}
