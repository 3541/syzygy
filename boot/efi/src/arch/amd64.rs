/*
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
    mem::{size_of, MaybeUninit},
    slice,
};

use r_efi::efi::BootServices;
use util::constants::MB;

use crate::{load::Image, log::Log, uefi::Pages, Result};

const PAGE_SIZE: usize = 0x1000;

fn chunks<T, const N: usize>(data: &mut [u8]) -> [&mut [T]; N] {
    assert_eq!(data.len() % N, 0);
    assert_eq!(data.len() / N % size_of::<T>(), 0);

    let mut res = [const { MaybeUninit::<&mut [T]>::uninit() }; N];
    for (i, chunk) in data.chunks_mut(data.len() / N).enumerate() {
        res[i].write(unsafe {
            slice::from_raw_parts_mut(chunk.as_mut_ptr() as *mut T, chunk.len() / size_of::<T>())
        });
    }

    unsafe { MaybeUninit::array_assume_init(res) }
}

pub fn map_image(log: &mut Log, bs: &BootServices, image: &Image) -> Result<Pages> {
    assert!(
        image.data.len() <= 2 * MB,
        "Quick and dirty bootstrap mapping assumes kernel <= 2 MB. Image is {:#x}.",
        image.data.len()
    );

    let mut tables = Pages::new_count(bs, 4)?;
    tables.data().fill(0);

    let [pml4, pdp, pd, pt] = chunks::<usize, 4>(tables.data());
    assert_eq!(pml4.len(), 512);

    todo!()
}
