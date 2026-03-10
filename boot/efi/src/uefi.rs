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
    ffi::c_void,
    fmt::Write,
    mem::{MaybeUninit, forget, size_of_val},
    ops::Range,
    ptr, slice,
};

use arrayvec::ArrayVec;
use blake2::{Blake2b512, Digest};
use r_efi::{
    efi::{self, BootServices, Guid, MemoryDescriptor},
    protocols::{file, loaded_image, simple_file_system},
};

use crate::{Result, log::Log};

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

    for i in 0..size_res / descriptor_size {
        let desc = unsafe {
            ptr::read(buf.as_ptr().offset((i * descriptor_size) as isize) as *const MemoryDescriptor)
        };
    }

    Ok(MemoryMap { key })
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
