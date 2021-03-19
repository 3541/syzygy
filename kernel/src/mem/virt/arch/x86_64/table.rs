//! x86_64 page tables.

use alloc::vec::Vec;
use core::mem::forget;
use core::ops::{Deref, DerefMut, Index, IndexMut};
use core::ptr::{read_volatile, write_volatile};

use bitflags::bitflags;

use super::flush_all_mappings;
use crate::mem::virt::{Flush, FlushAll, MappingFlags, TActivePrimaryTable, Unflushed};
use crate::mem::{
    Address, Page, PageAllocator, PageRef, PageType, PhysicalAddress, VirtualAddress, VirtualRange,
};
use crate::util::register;
use crate::util::sync::Spinlock;

trait PagedAddress: Address {}

bitflags! {
    pub struct EntryFlags: usize {
        const PRESENT = 1;
        const WRITABLE = 1 << 1;
        const USER_ACCESSIBLE = 1 << 2;
        const WRITE_THROUGH = 1 << 3;
        const CACHE_DISABLED = 1 << 4;
        const ACCESSED = 1 << 5;
        const DIRTY = 1 << 6;
        const LARGE = 1 << 7;
        const GLOBAL = 1 << 8;
        const NO_EXECUTE = 1 << 63;
    }
}

impl From<MappingFlags> for EntryFlags {
    fn from(m: MappingFlags) -> Self {
        Self::from_bits(m.bits()).expect("Unexpected mapping flag.")
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct Entry(usize);

impl Entry {
    const ADDRESS_MASK: usize = 0x000F_FFFF_FFFF_F000;

    fn flags(&self) -> EntryFlags {
        // SAFETY: Reading from here is safe (see: read_volatile doc).
        unsafe { EntryFlags::from_bits_truncate(read_volatile(&self.0)) }
    }

    fn present(&self) -> bool {
        self.flags().contains(EntryFlags::PRESENT)
    }

    fn address(&self) -> Option<PhysicalAddress> {
        if self.present() {
            Some(PhysicalAddress::new(self.0 & Self::ADDRESS_MASK))
        } else {
            None
        }
    }

    unsafe fn set(&mut self, addr: PhysicalAddress, flags: EntryFlags) {
        assert_eq!(addr.raw() & Self::ADDRESS_MASK, 0);
        write_volatile(&mut self.0, addr.raw() | flags.bits());
    }

    unsafe fn clear(&mut self) {
        write_volatile(&mut self.0, 0);
    }
}

const TABLE_ENTRIES: usize = 512;

#[repr(transparent)]
pub struct Table<const L: usize> {
    entries: [Entry; TABLE_ENTRIES],
}

type LeafTable = Table<1>;
type RootTable = Table<4>;

impl<const L: usize> Table<L> {
    const RMAP_ADDRESS_SHIFT: usize = 9;
    const RMAP_INDEX_SHIFT: usize = 12;
    const RMAP_INDEX_MASK: usize = (1 << Self::RMAP_ADDRESS_SHIFT) - 1;

    const fn index_of(addr: VirtualAddress) -> usize {
        (addr.raw() >> (Self::RMAP_INDEX_SHIFT + Self::RMAP_ADDRESS_SHIFT * (L - 1)))
            & Self::RMAP_INDEX_MASK
    }

    fn address_of(&self, i: usize) -> VirtualAddress {
        VirtualAddress::new(
            ((self as *const _ as usize) << Self::RMAP_ADDRESS_SHIFT)
                | (i << Self::RMAP_INDEX_SHIFT),
        )
    }

    fn new<const P: usize>(
        page: Page,
        parent: &mut Table<P>,
        i: usize,
        flags: EntryFlags,
    ) -> Unflushed<Self> {
        // Sadly, this one can't be checked at compile-time yet.
        debug_assert_eq!(P, L + 1);
        debug_assert!(L < 4);
        assert!(
            !parent[i].present(),
            "Attempted to create a new table at an occupied index."
        );
        // SAFETY: This is safe, since it has been verified that no existing mapping is overridden.
        unsafe { parent[i].set(page.address(), flags) };
        // SAFETY: This is safe, since the address does actually refer to an object owned by the
        // parent (to which this function requires a mutable reference, which becomes valid once the
        // mapping is flushed.
        unsafe { Unflushed::new(parent.address_of(i)) }
    }
}

impl LeafTable {
    fn set(&mut self, from: VirtualAddress, to: PhysicalAddress, flags: EntryFlags) {
        let i = Self::index_of(from);
        assert!(
            !self[i].present(),
            "Attempted to overwrite PRESENT mapping when mapping {} -> {}.",
            from,
            to
        );

        // SAFETY: This is safe, since it has been verified that no existing mapping is overridden.
        unsafe { self[i].set(to, flags) };
    }

    fn phys_address(&self, addr: VirtualAddress) -> Option<PhysicalAddress> {
        self[Self::index_of(addr)].address()
    }

    unsafe fn clear(&mut self, addr: VirtualAddress) {
        let i = Self::index_of(addr);
        assert!(
            self[i].present(),
            "Attempted to unmap non-present mapping {}.",
            addr
        );

        self[i].clear();
    }
}

impl<const L: usize> Index<usize> for Table<L> {
    type Output = Entry;

    fn index(&self, i: usize) -> &Entry {
        &self.entries[i]
    }
}

impl<const L: usize> IndexMut<usize> for Table<L> {
    fn index_mut(&mut self, i: usize) -> &mut Entry {
        &mut self.entries[i]
    }
}

impl<const L: usize> Table<L>
where
    [(); L]: Sized,
    [(); L - 1]: Sized,
    [(); L - 2]: Sized,
{
    fn child_address(&self, addr: VirtualAddress) -> Option<VirtualAddress> {
        let i = Self::index_of(addr);
        if self[i].present() {
            Some(self.address_of(i))
        } else {
            None
        }
    }

    fn child(&self, addr: VirtualAddress) -> Option<&Table<{ L - 1 }>> {
        self.child_address(addr)
            .map(|a| unsafe { &*(a.raw() as *const _) })
    }

    fn child_mut(&mut self, addr: VirtualAddress) -> Option<&mut Table<{ L - 1 }>> {
        self.child_address(addr)
            .map(|a| unsafe { &mut *(a.raw() as *mut _) })
    }

    fn ensure_child(&mut self, addr: VirtualAddress) -> &mut Table<{ L - 1 }> {
        if let Some(_) = self.child_mut(addr) {
            self.child_mut(addr).unwrap()
        } else {
            let page = PageAllocator::the()
                .alloc()
                .expect("Unable to allocate page.");
            let child = Table::<{ L - 1 }>::new(
                page,
                self,
                Self::index_of(addr),
                EntryFlags::PRESENT | EntryFlags::WRITABLE,
            );
            child.flush()
        }
    }
}

impl RootTable {
    fn leaf(&self, addr: VirtualAddress) -> Option<&LeafTable> {
        self.child(addr)
            .and_then(|a| a.child(addr))
            .and_then(|a| a.child(addr))
    }

    fn leaf_mut(&mut self, addr: VirtualAddress) -> Option<&mut LeafTable> {
        self.child_mut(addr)
            .and_then(|a| a.child_mut(addr))
            .and_then(|a| a.child_mut(addr))
    }

    fn ensure_leaf(&mut self, addr: VirtualAddress) -> &mut LeafTable {
        self.ensure_child(addr)
            .ensure_child(addr)
            .ensure_child(addr)
    }
}

pub struct InactivePrimaryTable(Page);

impl InactivePrimaryTable {
    fn phys_address(&self) -> PhysicalAddress {
        self.0.address()
    }
}

impl Drop for InactivePrimaryTable {
    fn drop(&mut self) {
        panic!("Leaked InactivePrimaryTable.");
    }
}

pub struct ActivePrimaryTable(Spinlock<&'static mut RootTable>);
#[must_use = "Unused QuickMap"]
struct TempMap<'a, T>(&'a mut T);

impl<'a, T> TempMap<'a, T> {
    const ADDRESS: VirtualAddress = unsafe { VirtualAddress::new_unchecked(0xFFFF_DEAD_DEAD_0000) };

    fn map<'p>(
        t: &ActivePrimaryTable,
        to: &'p mut Page,
        flags: MappingFlags,
    ) -> Unflushed<'p, Self> {
        unsafe { Unflushed::from(t.map(Self::ADDRESS, to, flags)) }
    }

    /// SAFETY: Caller must guarantee that unmapping does not destroy any needed references.
    unsafe fn unmap(&mut self, t: &ActivePrimaryTable) -> Flush {
        let ret = t.unmap(VirtualAddress::from_ptr(self.0));
        forget(self);
        ret
    }
}

impl<T> Deref for TempMap<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> DerefMut for TempMap<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> Drop for TempMap<'_, T> {
    fn drop(&mut self) {
        panic!("Dropped mapped QuickMap.");
    }
}

impl ActivePrimaryTable {
    const ACTIVE_TABLE: *mut Table<4> = 0xFFFF_FFFF_FFFF_F000 as *mut _;

    fn map_temp<'p, T>(&self, to: &'p mut Page, flags: MappingFlags) -> Unflushed<'p, TempMap<T>> {
        TempMap::map(self, to, flags)
    }
}

impl TActivePrimaryTable for ActivePrimaryTable {
    type Inactive = InactivePrimaryTable;

    unsafe fn current_active() -> Self {
        Self(Spinlock::new(&mut *ActivePrimaryTable::ACTIVE_TABLE))
    }

    fn map(&self, from: VirtualAddress, to: &mut Page, flags: MappingFlags) -> Flush {
        assert!(from.is_aligned(Page::SIZE));
        assert!(from != TempMap::<usize>::ADDRESS);

        let mut t = self.0.lock();
        let table = t.ensure_leaf(from);
        table.set(
            from,
            to.address(),
            EntryFlags::from(flags) | EntryFlags::PRESENT,
        );
        Flush(from)
    }

    unsafe fn unmap(&self, addr: VirtualAddress) -> Flush {
        assert!(addr.is_aligned(Page::SIZE));

        let mut t = self.0.lock();
        let table = t
            .leaf_mut(addr)
            .expect("Tried to unmap, but there is no corresponding table.");

        table.clear(addr);
        Flush(addr)
    }

    fn with(&self, t: &mut InactivePrimaryTable, f: impl FnOnce(&Self)) {
        // SAFETY: This is safe, since it only affects the recursive page table mapping, which is
        // restored later on. The aliasing inside only lasts one line, and is not abused.
        let mut current_table_page = unsafe {
            Page::new(
                self.0.lock()[TABLE_ENTRIES - 1].address().unwrap(),
                PageType::Unallocated,
            )
        };
        let current_table_address = current_table_page.address();

        let current_table_remap = unsafe {
            let mut table = self.0.lock();

            // At this point, there are two mappings to the current PML4.
            let current_table_remap: &mut TempMap<RootTable> = self
                .map_temp(&mut current_table_page, MappingFlags::WRITABLE)
                .flush();

            // Now there is only one mapping.
            table[TABLE_ENTRIES - 1]
                .set(t.phys_address(), EntryFlags::PRESENT | EntryFlags::WRITABLE);

            current_table_remap
        };

        flush_all_mappings();
        f(self);

        // SAFETY: Again, this block creates and then undoes an aliased mapping.
        unsafe {
            current_table_remap[TABLE_ENTRIES - 1].set(
                current_table_address,
                EntryFlags::PRESENT | EntryFlags::WRITABLE,
            );
            let mut flush = FlushAll::new();
            flush.consume(current_table_remap.unmap(self));
            flush.flush();
        }
    }

    unsafe fn swap(&self, other: Self::Inactive) -> Self::Inactive {
        let _l = self.0.lock();

        let ret = InactivePrimaryTable(Page::new(
            PhysicalAddress::new(register::read::cr3() as usize),
            PageType::Allocated,
        ));

        register::write::cr3(other.0.address().raw() as u64);
        forget(other);

        ret
    }

    fn translate(&self, addr: VirtualAddress) -> Option<PhysicalAddress> {
        self.0.lock().leaf(addr).and_then(|t| t.phys_address(addr))
    }

    fn translate_range(&self, range: VirtualRange) -> Option<Vec<Option<PageRef>>> {
        let mut found_one = false;
        let ret = range
            .into_iter()
            .map(|a| {
                let r = self.translate(a).map(PageRef::new);
                if r.is_some() {
                    found_one = true
                };
                r
            })
            .collect();

        if found_one {
            Some(ret)
        } else {
            None
        }
    }
}
