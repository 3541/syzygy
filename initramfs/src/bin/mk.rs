use std::env;
use std::fs;
use std::mem::{size_of, transmute};
use std::path::PathBuf;
use std::slice;

use initramfs::{FileHeader, Header};

fn to_byte_vec<T>(value: T) -> Vec<u8> {
    let data = unsafe {
        slice::from_raw_parts(
            transmute::<*const T, *const u8>(&value as *const T),
            size_of::<T>(),
        )
    };
    data.to_vec()
}

fn main() {
    let dir = env::args().nth(1).expect("A base path must be provided.");
    let out = env::args().nth(2).expect("An out file must be provided.");

    let dir = fs::read_dir(dir).expect("First argument must be a directory");

    let mut files: Vec<PathBuf> = Vec::new();
    for file in dir {
        let file = file.unwrap();
        if file.file_type().unwrap().is_file() {
            files.push(file.path());
        }
    }

    let mut buf: Vec<u8> = Vec::new();
    // This is the offset from the END of the current file header to its
    // corresponding file.
    let mut content_offset = size_of::<FileHeader>() * (files.len() - 1);
    let mut file_contents: Vec<Vec<u8>> = Vec::new();

    buf.append(&mut to_byte_vec(Header {
        file_count: files.len(),
    }));

    // Build the headers and put in buf
    for file in files {
        let name = file.file_name().unwrap().to_string_lossy();
        assert!(name.as_bytes().len() <= 64);
        let contents = fs::read(&file).unwrap();
        let mut file_header = FileHeader {
            name: [b'\0'; 64],
            content_offset,
            len: contents.len(),
        };
        file_header.name[0..name.as_bytes().len()].clone_from_slice(name.as_bytes());
        buf.append(&mut to_byte_vec(file_header));

        content_offset += contents.len();
        file_contents.push(contents);

        if content_offset <= size_of::<FileHeader>() {
            break;
        }
        content_offset -= size_of::<FileHeader>();
    }

    // Put the actual file contents in buf
    for mut contents in file_contents {
        buf.append(&mut contents);
    }

    fs::write(out, buf).unwrap();
}
