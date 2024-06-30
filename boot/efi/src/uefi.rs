use core::{
    ffi::c_void,
    fmt::Write,
    mem::{size_of_val, MaybeUninit},
    ptr, slice,
};

use blake2::{Blake2b512, Digest};
use r_efi::{
    efi::{self, BootServices, Guid},
    protocols::{file, loaded_image, simple_file_system},
};

use crate::{log::Log, Result};

pub fn res(s: efi::Status) -> Result<()> {
    if s.is_error() {
        Err(s.into())
    } else {
        Ok(())
    }
}

unsafe fn handle_protocol<T>(h: efi::Handle, bs: &BootServices, mut guid: Guid) -> Result<&mut T> {
    let mut protocol = ptr::null_mut();
    res((bs.handle_protocol)(
        h,
        &mut guid as *mut _,
        &mut protocol as *mut _,
    ))?;

    Ok(unsafe { &mut *(protocol as *mut _) })
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
    res((volume_protocol.open_volume)(
        volume_protocol as *mut _,
        &mut volume as *mut _,
    ))?;

    Ok(unsafe { &*(volume as *mut _) })
}

pub unsafe fn open_file<'a>(
    volume: &'a file::Protocol,
    path: &[u16],
) -> Result<&'a file::Protocol> {
    assert_eq!(path.last(), Some(&0u16));

    let mut file = ptr::null_mut();
    res((volume.open)(
        volume as *const _ as *mut _,
        &mut file as *mut _,
        path.as_ptr() as *mut _,
        file::MODE_READ,
        file::READ_ONLY | file::HIDDEN | file::SYSTEM,
    ))?;

    Ok(unsafe { &*(file as *mut _) })
}

pub fn file_size(file: &file::Protocol) -> Result<usize> {
    let mut info = MaybeUninit::<file::Info<64>>::uninit();
    let mut size = size_of_val(&info);
    let mut guid = file::INFO_ID;
    res((file.get_info)(
        file as *const _ as *mut _,
        &mut guid as *mut _,
        &mut size as *mut _,
        info.as_mut_ptr() as *mut _,
    ))?;

    // SAFETY: Now initialized, given the previous call did not fail.
    let info = unsafe { info.assume_init() };

    Ok(info.file_size as usize)
}

pub struct Image<'a> {
    bs: &'a BootServices,
    buf: *mut c_void,
    size: usize,
}

impl Drop for Image<'_> {
    fn drop(&mut self) {
        let status = (self.bs.free_pool)(self.buf);
        assert_eq!(status, efi::Status::SUCCESS);
    }
}

impl<'a> Image<'a> {
    pub fn load(bs: &'a BootServices, file: &file::Protocol, size: usize) -> Result<Self> {
        let mut buf = ptr::null_mut();
        res((bs.allocate_pool)(
            efi::LOADER_DATA,
            size,
            &mut buf as *mut _,
        ))?;

        let mut read_size = size;
        res((file.read)(
            file as *const _ as *mut _,
            &mut read_size as *mut _,
            buf,
        ))?;
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

        writeln!(log, "Image hash: {r:x}.\r")?;
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
        todo!();
    }
}

impl Pages {
    pub fn new(bs: &BootServices, size: usize) -> Result<Self> {
        let mut phys = 0u64;
        let count = (size - 1) / EFI_PAGE_SIZE + 1;
        res((bs.allocate_pages)(
            efi::ALLOCATE_ANY_PAGES,
            efi::LOADER_DATA,
            count,
            &mut phys as *mut _,
        ))?;

        let ptr = phys as *mut u8;
        unsafe { ptr.write_bytes(0, count * EFI_PAGE_SIZE) };

        Ok(Self { ptr, count })
    }

    pub fn ptr(&self) -> *mut u8 {
        self.ptr
    }

    pub fn data(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.size()) }
    }

    pub fn size(&self) -> usize {
        self.count * EFI_PAGE_SIZE
    }
}
