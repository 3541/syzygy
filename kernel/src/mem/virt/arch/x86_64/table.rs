//! x86_64 page tables.

use alloc::vec::Vec;
use core::fmt;
use core::mem::forget;
use core::ops::{Deref, DerefMut, Index, IndexMut};
use core::ptr::{read_volatile, write_volatile};
use core::slice;

use bitflags::bitflags;
use log_crate::trace;

use super::flush_all_mappings;
use crate::mem::virt::{
    Flush, FlushAll, MappingFlags, TActivePrimaryTable, TInactivePrimaryTable, Unflushed,
};
use crate::mem::{
    align_up, size, Address, Page, PageAllocator, PageRef, PageType, PhysicalAddress,
    VirtualAddress, VirtualRange,
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
        let mut ret = Self::from_bits(m.bits()).expect("Unexpected mapping flag.");
        // MappingFlags uses an EXECUTABLE bit instead of NX.
        ret.toggle(EntryFlags::NO_EXECUTE);
        ret
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
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
        assert_eq!(
            self.0 & Self::ADDRESS_MASK,
            0,
            "Tried to overwrite an existing entry."
        );
        assert_eq!(
            addr.raw() & !Self::ADDRESS_MASK,
            0,
            "The given address has invalid bits set."
        );
        write_volatile(&mut self.0, addr.raw() | flags.bits());
    }

    unsafe fn clear(&mut self) {
        write_volatile(&mut self.0, 0);
    }
}

const TABLE_ENTRIES: usize = 512;
const TABLE_RMAP_INDEX: usize = TABLE_ENTRIES - 2;

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
    const OFFSET_MASK: usize = (1 << Self::RMAP_INDEX_SHIFT) - 1;

    const fn index_of(addr: VirtualAddress) -> usize {
        (addr.raw() >> (Self::RMAP_INDEX_SHIFT + Self::RMAP_ADDRESS_SHIFT * (L - 1)))
            & Self::RMAP_INDEX_MASK
    }

    fn address_of_table(&self, i: usize) -> VirtualAddress {
        // SAFETY: This is fine because the address is immediately canonicalized.
        unsafe {
            VirtualAddress::new_unchecked(
                ((self as *const _ as usize) << Self::RMAP_ADDRESS_SHIFT)
                    | (i << Self::RMAP_INDEX_SHIFT),
            )
        }
        .canonicalize()
    }

    fn new<const P: usize>(
        page: Page,
        parent: &mut Table<P>,
        i: usize,
        flags: EntryFlags,
    ) -> Unflushed<&mut Self> {
        // Sadly, this one can't be checked at compile-time yet.
        debug_assert_eq!(P, L + 1);
        debug_assert!(L < 4);
        assert!(
            !parent[i].present(),
            "Attempted to create a new table at an occupied index."
        );
        // SAFETY: This is safe, since it has been verified that no existing mapping is overridden.
        unsafe { parent[i].set(page.leak(), flags) };
        // SAFETY: This is safe, since the address does actually refer to an object owned by the
        // parent (to which this function requires a mutable reference, which becomes valid once the
        // mapping is flushed.
        unsafe { Unflushed::from_address(parent.address_of_table(i)) }
    }
}

impl<const L: usize> fmt::Display for Table<L> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Table<{}>({})", L, VirtualAddress::from_ptr(self))
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
        let entry = &self[Self::index_of(addr)];
        trace!("Got entry {:x?}.", entry);
        entry
            .address()
            .map(|a| a + (addr.raw() & Self::OFFSET_MASK))
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
            assert!(
                !self[i].flags().contains(EntryFlags::LARGE),
                "{}[{}] points to a large page.",
                self,
                i
            );
            Some(self.address_of_table(i))
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

    /// Use the physmap to ensure recursive mapping of the PML4. This should only be called once,
    /// while the bootloader's page tables are still in place.
    unsafe fn early_ensure_recursive_mapping(&mut self) {
        let pml4_addr = register::read::cr3();
        // Stivale only guarantees 4 GB of identity-mapped physical memory.
        assert!(
            (pml4_addr as usize) < 4 * size::GB,
            "PML4 unexpectedly high."
        );
        trace!("Bootstrap PML4 at P:0x{:x}.", pml4_addr);

        let pml4: &mut [Entry] = slice::from_raw_parts_mut(
            (crate::consts::PHYS_BASE + pml4_addr as usize).as_mut_ptr(),
            TABLE_ENTRIES,
        );
        pml4[TABLE_RMAP_INDEX].clear();
        pml4[TABLE_RMAP_INDEX].set(
            PhysicalAddress::new(pml4_addr as usize),
            EntryFlags::WRITABLE | EntryFlags::PRESENT,
        );
        flush_all_mappings();
        trace!("INITIALIZED bootstrap PML4 recursive mapping.");
    }
}

pub struct InactivePrimaryTable(Page);

impl InactivePrimaryTable {
    fn ensure_recursive_mapping(&mut self, active: &ActivePrimaryTable) {
        let mut table: TempMap<RootTable> =
            active.map_temp(&mut self.0, MappingFlags::WRITABLE).flush();
        // SAFETY: This will panic if it overwrites an existing mapping.
        unsafe {
            table[TABLE_RMAP_INDEX]
                .set(self.0.address(), EntryFlags::PRESENT | EntryFlags::WRITABLE);
            table.unmap(active).flush();
        };
    }
}

impl TInactivePrimaryTable for InactivePrimaryTable {
    fn new(p: Page) -> Self {
        Self(p)
    }

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
#[must_use = "Unused TempMap"]
struct TempMap<'a, T>(&'a mut T);

impl<'a, T> TempMap<'a, T> {
    const ADDRESS: VirtualAddress = unsafe { VirtualAddress::new_unchecked(0xFFFF_DEAD_DEAD_0000) };

    fn map<'p>(t: &ActivePrimaryTable, to: &'p mut Page, flags: MappingFlags) -> Unflushed<Self> {
        unsafe {
            Unflushed::new(
                Self(&mut *Self::ADDRESS.as_mut_ptr()),
                t.map_unchecked(Self::ADDRESS, to, flags),
            )
        }
    }

    /// SAFETY: Caller must guarantee that unmapping does not destroy any needed references.
    unsafe fn unmap(self, t: &ActivePrimaryTable) -> Flush {
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
        panic!("Dropped mapped TempMap.");
    }
}

impl ActivePrimaryTable {
    const ACTIVE_TABLE: *mut RootTable = 0xFFFF_FF7F_BFDF_E000 as *mut _;

    fn map_temp<T>(&self, to: &mut Page, flags: MappingFlags) -> Unflushed<TempMap<T>> {
        TempMap::map(self, to, flags)
    }

    unsafe fn map_unchecked(&self, from: VirtualAddress, to: &Page, flags: MappingFlags) -> Flush {
        let mut t = self.0.lock();
        let table = t.ensure_leaf(from);
        table.set(
            from,
            to.address(),
            EntryFlags::from(flags) | EntryFlags::PRESENT,
        );
        Flush(from)
    }
}

impl TActivePrimaryTable for ActivePrimaryTable {
    type Inactive = InactivePrimaryTable;

    unsafe fn current_active() -> Self {
        let ret = Self(Spinlock::new(&mut *ActivePrimaryTable::ACTIVE_TABLE));
        ret
    }

    fn map(&self, from: VirtualAddress, to: &Page, flags: MappingFlags) -> Flush {
        assert!(from.is_aligned(Page::SIZE));
        assert!(from != TempMap::<usize>::ADDRESS);
        assert_ne!(RootTable::index_of(from), TABLE_RMAP_INDEX);
        unsafe { self.map_unchecked(from, to, flags) }
    }

    fn map_range(
        &self,
        from: VirtualRange,
        to: Vec<Option<&Page>>,
        flags: MappingFlags,
    ) -> Vec<Flush> {
        assert_eq!(align_up(from.size(), Page::SIZE) / Page::SIZE, to.len());

        from.into_iter()
            .zip(to.into_iter())
            .flat_map(|(addr, page)| Some(self.map(addr, page?, flags)))
            .collect()
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
                self.0.lock()[TABLE_RMAP_INDEX].address().unwrap(),
                PageType::Unallocated,
            )
        };
        let current_table_address = current_table_page.address();

        t.ensure_recursive_mapping(self);

        let mut current_table_remap = unsafe {
            // At this point, there are two mappings to the current PML4.
            let mut current_table_remap: TempMap<RootTable> = self
                .map_temp(&mut current_table_page, MappingFlags::WRITABLE)
                .flush();

            let mut _table_lock = self.0.lock();
            current_table_remap[TABLE_RMAP_INDEX].clear();
            // Now there is only one mapping, and the recursive mapping in the current PML4 points
            // to the table we want to edit.
            current_table_remap[TABLE_RMAP_INDEX]
                .set(t.phys_address(), EntryFlags::PRESENT | EntryFlags::WRITABLE);

            current_table_remap
        };

        flush_all_mappings();
        f(self);

        // SAFETY: Again, this block creates and then undoes an aliased mapping.
        unsafe {
            current_table_remap[TABLE_RMAP_INDEX].clear();
            current_table_remap[TABLE_RMAP_INDEX].set(
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

        trace!("About to swap PML4.");
        register::write::cr3(other.0.address().raw() as u64);
        trace!("Swapped to new PML4.");
        forget(other);

        ret
    }

    fn translate(&self, addr: VirtualAddress) -> Option<PhysicalAddress> {
        self.0.lock().leaf(addr).and_then(|t| {
            trace!("Got leaf table {} (0b{:b}).", t, t as *const _ as usize);
            t.phys_address(addr)
        })
    }

    fn translate_range(&self, range: VirtualRange) -> Option<Vec<Option<PageRef>>> {
        let mut found_one = false;
        let ret = range
            .into_iter()
            .map(|a| {
                let r = self.translate(a).map(PageRef::new);
                if r.is_some() {
                    found_one = true;
                    trace!("Translated {} to {}.", a, r.as_ref().unwrap().address());
                } else {
                    trace!("Failed to translate {}.", a);
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

pub fn init() -> ActivePrimaryTable {
    // SAFETY: This is the only reader of the bootstrap tables.
    let table = unsafe { ActivePrimaryTable::current_active() };
    unsafe { table.0.lock().early_ensure_recursive_mapping() };
    table
}
