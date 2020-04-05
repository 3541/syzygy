use super::mapper::Mapper;
use super::table::{ActiveTopLevelTable, Table, PML4};
use super::{EntryFlags, Page};
use crate::memory::Frame;

pub struct TempPage {
    pub page: Page,
    //    allocator: TempAllocator,
}

impl TempPage {
    pub fn map(&mut self, active: &mut ActiveTopLevelTable) {
        assert!(active.translate_page(self.page.address()).is_none());
        active.map_to(self.page.address(), self.page.frame, EntryFlags::WRITABLE);
    }

    pub fn unmap(mut self, active: &mut Mapper) {
        active.unmap(self.page);
    }

    pub fn map_and_pun_frame(&mut self, active: &mut ActiveTopLevelTable) -> &mut Table<PML4> {
        self.map(active);
        unsafe { &mut *(*self.page.address() as *mut Table<PML4>) }
    }

    pub fn new(page: Page) -> TempPage {
        TempPage {
            page,
            //            allocator: TempAllocator::new(allocator),
        }
    }
}

/*struct TempAllocator([Option<Frame>; 3]);

impl TempAllocator {
    fn new(allocator: &mut impl FrameAllocator) -> TempAllocator {
        let mut a = || allocator.alloc();
        TempAllocator([a(), a(), a()])
    }
}

impl FrameAllocator for TempAllocator {
    fn alloc(&mut self) -> Option<Frame> {
        for f in &mut self.0 {
            if f.is_some() {
                return f.take();
            }
        }
        None
    }

    fn free(&mut self, frame: Frame) {
        for f in &mut self.0 {
            if f.is_none() {
                *f = Some(frame);
                return;
            }
        }
        panic!("Tried to free more than TempAllocator can fit. This shouldn't happen at all.");
    }
}
*/
