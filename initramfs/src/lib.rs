#![no_std]

extern crate alloc;

use alloc::string::String;

use core::mem::{size_of, transmute};

use hashbrown::HashMap;

#[repr(C)]
pub struct File<'a>(&'a [u8]);

#[repr(C)]
pub struct Initramfs<'a>(pub HashMap<String, &'a File<'a>>);

#[repr(C)]
pub struct Header {
    pub file_count: usize,
}

#[repr(C)]
pub struct FileHeader {
    pub name: [u8; 64],
    // NOTE that this offset is from the END of this struct
    pub content_offset: usize,
}

fn end_index(haystack: &[u8]) -> usize {
    for (i, v) in haystack.iter().enumerate() {
        if *v == b'\0' {
            return i;
        }
    }
    haystack.len() - 1
}

impl Initramfs<'_> {
    pub unsafe fn new(data: &'static [u8]) -> Self {
        let header: &Header = transmute(data.as_ptr());
        let mut data = &data[size_of::<Header>()..];

        let mut files = HashMap::new();

        for _ in 0..header.file_count {
            let file_header: &FileHeader = transmute(data.as_ptr());
            data = &data[size_of::<FileHeader>()..];

            files.insert(
                String::from_utf8_unchecked(
                    (&file_header.name[0..end_index(&file_header.name)]).to_vec(),
                ),
                transmute(&data[file_header.content_offset..].as_ptr()),
            );
        }

        Self(files)
    }
}
