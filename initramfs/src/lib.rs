#![no_std]

extern crate alloc;

use alloc::string::String;

use core::mem::size_of;
use core::ops::Deref;

use hashbrown::HashMap;

#[repr(C)]
pub struct File<'a>(&'a [u8]);

impl<'a> Deref for File<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[repr(C)]
pub struct Header {
    pub file_count: usize,
}

#[repr(C)]
pub struct FileHeader {
    pub name: [u8; 64],
    // NOTE that this offset is from the END of this struct
    pub content_offset: usize,
    pub len: usize,
}

fn end_index(haystack: &[u8]) -> usize {
    for (i, v) in haystack.iter().enumerate() {
        if *v == b'\0' {
            return i;
        }
    }
    haystack.len() - 1
}

#[repr(C)]
pub struct Initramfs<'a>(pub HashMap<String, File<'a>>);

impl Initramfs<'_> {
    /// # Safety
    /// Must be called with `data` a valid buffer created by `mk`.
    #[allow(clippy::cast_ptr_alignment)]
    pub unsafe fn new(data: &'static [u8]) -> Self {
        let header: &Header = &*(data.as_ptr() as *const _);
        let mut data = &data[size_of::<Header>()..];

        let mut files = HashMap::new();

        for _ in 0..header.file_count {
            let file_header: &FileHeader = &*(data.as_ptr() as *const _);
            data = &data[size_of::<FileHeader>()..];

            files.insert(
                String::from_utf8_unchecked(
                    (&file_header.name[0..end_index(&file_header.name)]).to_vec(),
                ),
                File(
                    &data[file_header.content_offset..file_header.content_offset + file_header.len],
                ),
            );
        }

        Self(files)
    }
}
