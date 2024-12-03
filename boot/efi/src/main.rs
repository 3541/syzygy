/*
 * EFI: EFI boot loader.
 *
 * This is a very simple EFI bootloader. It will:
 * 1. Load the kernel from the filename 'sz' on the same volume as the loader itself.
 * 2. Relocate to a random address.
 * 3. Set up a bootstrap page mapping.
 * 4. Apply any necessary architecture-specific configuration (e.g., WP bit).
 * 5. Invoke kinit(), passing the memory map and bootstrap page tables.
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

#![no_main]
#![no_std]
#![feature(maybe_uninit_array_assume_init)]

mod arch;
mod load;
mod log;
#[cfg(target_arch = "aarch64")]
mod rand;
mod uefi;

use core::fmt::{self, Write};

use log::{Log, LOG};
use r_efi::efi;
use ucs2::ucs2_cstr;

use arch::map_image;
use load::Image;
use uefi::{file_size, open_file, open_image_volume, FileImage};

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    if let Some(mut log) = unsafe { LOG } {
        let _ = write!(log, "PANIC: \r\n{}", info.message());
    }

    loop {}
}

enum Error {
    Efi(efi::Status),
    Elf(elf::ParseError),
    Load(load::Error),
    Fmt,
}

impl From<efi::Status> for Error {
    fn from(value: efi::Status) -> Self {
        Self::Efi(value)
    }
}

impl From<fmt::Error> for Error {
    fn from(_: fmt::Error) -> Self {
        Self::Fmt
    }
}

impl From<elf::ParseError> for Error {
    fn from(value: elf::ParseError) -> Self {
        Self::Elf(value)
    }
}

impl From<load::Error> for Error {
    fn from(value: load::Error) -> Self {
        Self::Load(value)
    }
}

type Result<T> = core::result::Result<T, Error>;

const EXPECTED_HASH: &str = env!("SZ_KERNEL_HASH");
fn expected_hash() -> [u8; 64] {
    let mut ret = [0u8; 64];
    hex::decode_to_slice(EXPECTED_HASH, &mut ret).unwrap();

    ret
}

fn start(image: efi::Handle, st: &mut efi::SystemTable) -> Result<()> {
    let mut log = unsafe { LOG.unwrap() };

    log.clear()?;
    writeln!(log, "Syzygy EFI loader {}.\r", env!("SZ_VER"))?;

    let bs = unsafe { &*st.boot_services };
    let volume = unsafe { open_image_volume(image, bs) }?;
    let kernel = unsafe { open_file(volume, &ucs2_cstr!("sz")) }?;
    let size = file_size(kernel)?;
    writeln!(log, "Found kernel image, {size} bytes.\r")?;

    let image = FileImage::load(bs, kernel, size)?;
    writeln!(log, "Loaded image, verifying...\r")?;

    let hash = image.hash(log)?;
    let expected = expected_hash();

    if hash != expected {
        panic!("Hash mismatch. Found {hash:x?}, expected {expected:x?}");
    }
    writeln!(log, "Hash matches {EXPECTED_HASH}.\r")?;

    let image = Image::load(&mut log, bs, &image)?;
    map_image(&mut log, bs, &image)?;

    todo!("Final setup and jump");
}

#[no_mangle]
pub extern "efiapi" fn efi_main(image: efi::Handle, st: &mut efi::SystemTable) -> efi::Status {
    unsafe { Log::init(st) };

    match start(image, st) {
        Ok(_) => {
            // Wait for key input, by waiting on the `wait_for_key` event hook.
            let r = unsafe {
                let mut x: usize = 0;
                ((*st.boot_services).wait_for_event)(1, &mut (*st.con_in).wait_for_key, &mut x)
            };

            if r.is_error() {
                r
            } else {
                efi::Status::SUCCESS
            }
        },
        Err(Error::Efi(s)) => panic!("EFI error: {s:?}\r"),
        Err(Error::Elf(e)) => panic!("ELF parsing error: {e}\r"),
        Err(Error::Load(e)) => panic!("ELF loading error: {e}\r"),
        Err(Error::Fmt) => efi::Status::DEVICE_ERROR, // Panic probably a bad idea.
    }
}
