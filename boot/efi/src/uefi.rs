/*
 * UEFI: EFI protocol bindings.
 *
 * Copyright (c) 2024 Alex O'Brien <3541@3541.website>
 *
 * This file is part of Syzygy.
 *
 * Syzygy is free software: you can redistribute it and/or modify it under the
 * terms of version 3 the GNU General Public License as published by the Free
 * Software Foundation.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software. If not, see <https://www.gnu.org/licenses/>.
 */

use core::{
    cmp::{max, min},
    ffi::c_void,
    fmt::Write,
    mem::{MaybeUninit, forget, size_of_val},
    ops::Range,
    ptr, slice,
};

use arrayvec::ArrayVec;
use blake2::{Blake2b512, Digest};
use r_efi::{
    efi::{
        self, ACPI_MEMORY_NVS, ACPI_RECLAIM_MEMORY, BOOT_SERVICES_CODE, BOOT_SERVICES_DATA,
        BootServices, CONVENTIONAL_MEMORY, Guid, LOADER_CODE, LOADER_DATA, MEMORY_MAPPED_IO,
        MEMORY_MAPPED_IO_PORT_SPACE, MemoryDescriptor, PAL_CODE, PERSISTENT_MEMORY,
        RESERVED_MEMORY_TYPE, RUNTIME_SERVICES_CODE, RUNTIME_SERVICES_DATA, UNACCEPTED_MEMORY_TYPE,
        UNUSABLE_MEMORY,
    },
    protocols::{file, loaded_image, simple_file_system},
};

use crate::{Result, log::Log};
use common::mmap::{MmapEntry, MmapEntryType};

pub fn res(s: efi::Status) -> Result<()> {
    if s.is_error() { Err(s.into()) } else { Ok(()) }
}

unsafe fn handle_protocol<T>(h: efi::Handle, bs: &BootServices, mut guid: Guid) -> Result<&mut T> {
    let mut protocol = ptr::null_mut();
    unsafe {
        res((bs.handle_protocol)(
            h,
            &mut guid as *mut _,
            &mut protocol as *mut _,
        ))?;
        Ok(&mut *(protocol as *mut _))
    }
}

pub unsafe fn open_image_volume(image: efi::Handle, bs: &BootServices) -> Result<&file::Protocol> {
    let li_protocol = unsafe {
        handle_protocol::<loaded_image::Protocol>(image, bs, loaded_image::PROTOCOL_GUID)
    }?;
    let volume_protocol = unsafe {
        handle_protocol::<simple_file_system::Protocol>(
            li_protocol.device_handle,
            bs,
            simple_file_system::PROTOCOL_GUID,
        )
    }?;

    let mut volume = ptr::null_mut();

    unsafe {
        res((volume_protocol.open_volume)(
            volume_protocol as *mut _,
            &mut volume as *mut _,
        ))?;

        Ok(&*(volume as *mut _))
    }
}

pub unsafe fn open_file<'a>(
    volume: &'a file::Protocol,
    path: &[u16],
) -> Result<&'a file::Protocol> {
    assert_eq!(path.last(), Some(&0u16));

    let mut file = ptr::null_mut();

    unsafe {
        res((volume.open)(
            volume as *const _ as *mut _,
            &mut file as *mut _,
            path.as_ptr() as *mut _,
            file::MODE_READ,
            file::READ_ONLY | file::HIDDEN | file::SYSTEM,
        ))?;

        Ok(&*(file as *mut _))
    }
}

pub fn file_size(file: &file::Protocol) -> Result<usize> {
    let mut info = MaybeUninit::<file::Info<64>>::uninit();
    let mut size = size_of_val(&info);
    let mut guid = file::INFO_ID;
    unsafe {
        res((file.get_info)(
            file as *const _ as *mut _,
            &mut guid as *mut _,
            &mut size as *mut _,
            info.as_mut_ptr() as *mut _,
        ))?;
    }

    // SAFETY: Now initialized, given the previous call did not fail.
    let info = unsafe { info.assume_init() };

    Ok(info.file_size as usize)
}

pub struct FileImage<'a> {
    bs: &'a BootServices,
    buf: *mut c_void,
    size: usize,
}

impl Drop for FileImage<'_> {
    fn drop(&mut self) {
        let status = unsafe { (self.bs.free_pool)(self.buf) };
        assert_eq!(status, efi::Status::SUCCESS);
    }
}

impl<'a> FileImage<'a> {
    pub fn load(bs: &'a BootServices, file: &file::Protocol, size: usize) -> Result<Self> {
        let mut buf = ptr::null_mut();
        unsafe {
            res((bs.allocate_pool)(
                efi::LOADER_DATA,
                size,
                &mut buf as *mut _,
            ))?;
        }

        let mut read_size = size;
        unsafe {
            res((file.read)(
                file as *const _ as *mut _,
                &mut read_size as *mut _,
                buf,
            ))?;
        }
        assert_eq!(read_size, size);

        Ok(Self { bs, buf, size })
    }

    pub fn data(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.buf as *const _, self.size) }
    }

    pub fn hash(&self, mut log: Log) -> Result<[u8; 64]> {
        let mut h = Blake2b512::new();
        h.update(self.data());
        let r = h.finalize();
        let mut ret = [0u8; 64];
        ret.clone_from_slice(&r);

        writeln!(log, "Image hash: {r:x}.")?;
        Ok(ret)
    }
}

pub const EFI_PAGE_SIZE: usize = 0x1000;

pub struct Pages {
    ptr: *mut u8,
    count: usize,
}

impl Drop for Pages {
    fn drop(&mut self) {
        panic!("Did not free pages.");
    }
}

impl Pages {
    pub fn new(bs: &BootServices, size: usize) -> Result<Self> {
        Self::new_count(bs, (size - 1) / EFI_PAGE_SIZE + 1)
    }

    pub fn new_count(bs: &BootServices, count: usize) -> Result<Self> {
        let mut phys = 0u64;
        unsafe {
            res((bs.allocate_pages)(
                efi::ALLOCATE_ANY_PAGES,
                efi::LOADER_DATA,
                count,
                &mut phys as *mut _,
            ))?;
        }

        let ptr = phys as *mut u8;
        unsafe { ptr.write_bytes(0, count * EFI_PAGE_SIZE) };

        Ok(Self { ptr, count })
    }

    pub fn ptr(&self) -> *mut u8 {
        self.ptr
    }

    pub fn data(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.len()) }
    }

    pub fn len(&self) -> usize {
        self.count * EFI_PAGE_SIZE
    }

    pub fn leak(self) -> &'static mut [u8] {
        let res = unsafe { slice::from_raw_parts_mut(self.ptr, self.len()) };
        forget(self);
        res
    }
}

pub struct MemoryMap {
    pub key: usize,
    pub map: ArrayVec<MmapEntry, 32>,
    pub max_usable_range: Range<usize>,
}

fn mmap_type(t: u32) -> MmapEntryType {
    match t {
        RESERVED_MEMORY_TYPE
        | RUNTIME_SERVICES_CODE
        | RUNTIME_SERVICES_DATA
        | UNUSABLE_MEMORY
        | ACPI_MEMORY_NVS
        | MEMORY_MAPPED_IO
        | MEMORY_MAPPED_IO_PORT_SPACE
        | PAL_CODE => MmapEntryType::Reserved,
        LOADER_CODE | LOADER_DATA | BOOT_SERVICES_CODE | BOOT_SERVICES_DATA
        | CONVENTIONAL_MEMORY => MmapEntryType::Usable,
        ACPI_RECLAIM_MEMORY => MmapEntryType::ACPIReclaimable,
        PERSISTENT_MEMORY | UNACCEPTED_MEMORY_TYPE => MmapEntryType::Reserved, // Not entirely sure what these are.
        _ => MmapEntryType::Reserved,
    }
}

fn memory_map(bs: &BootServices, kernel_range: Range<*const u8>) -> Result<MemoryMap> {
    let mut buf = [0u8; 8192];
    let size = size_of_val(&buf);

    let mut size_res = size;
    let mut key = 0usize;
    let mut descriptor_size = 0usize;
    let mut descriptor_version = 0u32;
    unsafe {
        res((bs.get_memory_map)(
            &mut size_res,
            buf.as_mut_ptr() as *mut MemoryDescriptor,
            &mut key,
            &mut descriptor_size,
            &mut descriptor_version,
        ))?;
    }

    assert!(descriptor_size >= size_of::<MemoryDescriptor>());
    assert!(size_res <= size);

    let mut map: ArrayVec<MmapEntry, 32> = ArrayVec::new();

    let mut min_usable = usize::MAX;
    let mut max_usable = 0usize;
    let mut add_entry = |entry: MmapEntry| {
        if entry.entry_type == MmapEntryType::Usable {
            min_usable = min(min_usable, entry.start_phys);
            max_usable = max(max_usable, entry.end_phys());
        }

        if let Some(last) = map.last_mut()
            && last.entry_type == entry.entry_type
            && last.end_phys() == entry.start_phys
        {
            last.size += entry.size;
        } else {
            map.push(entry);
        }
    };

    for i in 0..size_res / descriptor_size {
        let desc = unsafe {
            ptr::read_volatile(
                buf.as_ptr().offset((i * descriptor_size) as isize) as *const MemoryDescriptor
            )
        };

        let start = desc.physical_start as usize;
        let size = desc.number_of_pages as usize * EFI_PAGE_SIZE;
        let end = start + size;
        let entry_type = mmap_type(desc.r#type);

        if start <= kernel_range.start as usize && (kernel_range.start as usize) < end {
            assert!(kernel_range.end as usize <= end);

            if start < kernel_range.start as usize {
                add_entry(MmapEntry {
                    entry_type,
                    start_phys: start,
                    size: kernel_range.start as usize - start,
                });
            }
            add_entry(MmapEntry {
                entry_type: MmapEntryType::Kernel,
                start_phys: kernel_range.start as usize,
                size: kernel_range.end as usize - kernel_range.start as usize,
            });
            if (kernel_range.end as usize) < end {
                add_entry(MmapEntry {
                    entry_type,
                    start_phys: kernel_range.end as usize,
                    size: end - kernel_range.end as usize,
                });
            }
        } else {
            add_entry(MmapEntry {
                entry_type,
                start_phys: start,
                size,
            })
        }
    }

    Ok(MemoryMap {
        key,
        map,
        max_usable_range: min_usable..max_usable,
    })
}

pub fn exit_boot_services(
    bs: &BootServices,
    image: efi::Handle,
    kernel_range: Range<*const u8>,
) -> Result<MemoryMap> {
    let mut err = crate::Error::Efi(efi::Status::SUCCESS);
    const ATTEMPTS: usize = 2;
    for _ in 0..ATTEMPTS {
        let map = memory_map(bs, kernel_range.clone())?;
        match unsafe { res((bs.exit_boot_services)(image, map.key)) } {
            Ok(()) => return Ok(map),
            Err(e) => err = e,
        }
    }

    Err(err)
}
