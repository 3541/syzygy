use core::mem::size_of;
use core::ops::{Index, IndexMut};
use core::slice;

#[thread_local]
static mut GDT: Gdt = Gdt(&mut [Entry::null(), Entry::null(), Entry::null()]);

#[derive(Copy, Clone)]
#[repr(u16)]
enum GdtIndex {
    Null,
    /*KernelCode,
    KernelData,
        UserData,
    UserCode,*/
}

#[repr(C, packed)]
struct GdtPointer {
    size: u16,
    address: u64,
}

#[derive(Copy, Clone)]
#[repr(C, packed)]
struct Entry {
    limit_low: u16,
    base_low: u16,
    base_mid: u8,
    access: u8,
    limit_high_flags: u8,
    base_high: u8,
}

impl Entry {
    const fn null() -> Entry {
        Entry {
            limit_low: 0,
            base_low: 0,
            base_mid: 0,
            access: 0,
            limit_high_flags: 0,
            base_high: 0,
        }
    }

    /*    fn set_base(&mut self, base: VirtualAddress) {
        let base = *base;
        self.base_low = base as u16;
        self.base_mid = (base >> 16) as u8;
        self.base_high = (base >> 24) as u8;
    }*/
}

struct Gdt(pub &'static mut [Entry]);

impl Gdt {
    fn current() -> Gdt {
        let mut pointer = GdtPointer {
            size: 0,
            address: 0,
        };
        unsafe {
            asm!("sgdt [{}]", in(reg) &mut pointer);
            Gdt(slice::from_raw_parts_mut(
                pointer.address as *mut Entry,
                (pointer.size + 1) as usize / size_of::<Entry>(),
            ))
        }
    }

    /// # Safety
    /// Caller must ensure that the given GDT is valid and mapped.
    unsafe fn load(&self) {
        let p = GdtPointer {
            size: (size_of::<Entry>() * self.0.len() - 1) as u16,
            address: &self[GdtIndex::Null] as *const _ as u64,
        };

        asm!("lgdt [{}]", in(reg) &p);
    }
}

impl Index<GdtIndex> for Gdt {
    type Output = Entry;

    fn index(&self, i: GdtIndex) -> &Entry {
        &self.0[i as usize]
    }
}

impl IndexMut<GdtIndex> for Gdt {
    fn index_mut(&mut self, i: GdtIndex) -> &mut Entry {
        &mut self.0[i as usize]
    }
}

pub fn init() {
    // Copy over the initial GDT into one which can be changed.
    unsafe {
        GDT.0.copy_from_slice(Gdt::current().0);
        GDT.load();
    }
}
