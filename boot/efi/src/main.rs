#![no_main]
#![no_std]
#![feature(panic_info_message)]

use core::{
    ffi::c_void,
    fmt::{self, Write},
    mem::{size_of_val, MaybeUninit},
    ptr, slice,
};

use blake2::{Blake2b512, Digest};
use r_efi::efi::{
    self,
    protocols::{
        file, loaded_image, simple_file_system,
        simple_text_output::{self, ProtocolClearScreen, ProtocolOutputString},
    },
    BootServices, Guid,
};
use spin::Once;
use ucs2::ucs2_cstr;

static LOG: Once<Log> = Once::new();

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    if let Some(log) = LOG.get() {
        let mut log = *log;
        let _ = write!(log, "PANIC: ");

        if let Some(msg) = info.message() {
            let _ = fmt::write(&mut log, *msg);
            let _ = write!(log, "\r\n");
        } else {
            let _ = writeln!(log, "<no message>\r");
        }
    }

    loop {}
}

enum Error {
    Efi(efi::Status),
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

type Result<T> = core::result::Result<T, Error>;

fn res(s: efi::Status) -> Result<()> {
    if s.is_error() {
        Err(s.into())
    } else {
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct Log {
    print: ProtocolOutputString,
    clear: ProtocolClearScreen,
    protocol: usize, // *mut simple_text_protocol::Protocol, but Send. Shhh....
}

impl Log {
    fn new(st: &mut efi::SystemTable) -> Log {
        unsafe {
            Log {
                print: (*st.con_out).output_string,
                clear: (*st.con_out).clear_screen,
                protocol: st.con_out as usize,
            }
        }
    }

    fn clear(&self) -> Result<()> {
        res((self.clear)(self.protocol()))
    }

    fn print(&self, str: &str) -> Result<()> {
        let mut status = efi::Status::SUCCESS;

        ucs2::encode_with(str, |ch| {
            let mut buf = [ch, 0];

            status = (self.print)(self.protocol(), &mut buf as *mut _);
            if status.is_error() {
                Err(ucs2::Error::BufferOverflow)
            } else {
                Ok(())
            }
        })
        .map_err(|_| Error::Efi(status))
    }

    fn protocol(&self) -> *mut simple_text_output::Protocol {
        self.protocol as *mut _
    }
}

impl Write for Log {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.print(s).map_err(|_| fmt::Error)
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

unsafe fn open_image_volume(image: efi::Handle, bs: &BootServices) -> Result<&file::Protocol> {
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

unsafe fn open_file<'a>(volume: &'a file::Protocol, path: &[u16]) -> Result<&'a file::Protocol> {
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

fn file_size(file: &file::Protocol) -> Result<usize> {
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

struct Image<'a> {
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
    fn load(bs: &'a BootServices, file: &file::Protocol, size: usize) -> Result<Self> {
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

    fn data(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.buf as *const _, self.size) }
    }

    fn hash(&self, mut log: Log) -> Result<[u8; 64]> {
        let mut h = Blake2b512::new();
        h.update(self.data());
        let r = h.finalize();
        let mut ret = [0u8; 64];
        ret.clone_from_slice(&r);

        writeln!(log, "Image hash: {r:x}.\r")?;
        Ok(ret)
    }
}

const EXPECTED_HASH: &str = env!("SZ_KERNEL_HASH");
fn expected_hash() -> [u8; 64] {
    let mut ret = [0u8; 64];
    hex::decode_to_slice(EXPECTED_HASH, &mut ret).unwrap();

    ret
}

fn start(image: efi::Handle, st: &mut efi::SystemTable) -> Result<()> {
    let mut log = *LOG.get().unwrap();

    log.clear()?;
    writeln!(log, "Syzygy EFI loader {}.\r", env!("CARGO_PKG_VERSION"))?;

    let bs = unsafe { &*st.boot_services };
    let volume = unsafe { open_image_volume(image, bs) }?;
    let kernel = unsafe { open_file(volume, &ucs2_cstr!("sz")) }?;
    let size = file_size(kernel)?;
    writeln!(log, "Found kernel image, {size} bytes.\r")?;

    let image = Image::load(bs, kernel, size)?;
    writeln!(log, "Loaded image, verifying...\r")?;

    let hash = image.hash(log)?;
    let expected = expected_hash();

    if hash != expected {
        panic!("Hash mismatch. Found {hash:x?}, expected {expected:x?}");
    }
    writeln!(log, "Hash matches {EXPECTED_HASH}.\r")?;

    Ok(())
}

#[no_mangle]
pub extern "efiapi" fn efi_main(image: efi::Handle, st: &mut efi::SystemTable) -> efi::Status {
    let mut log = *LOG.call_once(|| Log::new(st));

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
        }
        Err(Error::Efi(s)) => {
            let _ = writeln!(log, "Error: {s:?}\r");
            s
        }
        Err(Error::Fmt) => efi::Status::DEVICE_ERROR,
    }
}
