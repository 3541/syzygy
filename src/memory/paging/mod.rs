use memory::PAGE_SIZE;

const ENTRY_COUNT: usize = 512;

pub type PhysAddress = usize;
pub type VirtAddress = usize;

pub struct Page {
    number: usize,
}
