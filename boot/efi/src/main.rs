#![no_main]
#![no_std]

use core::fmt::{self, Write};

use r_efi::efi::{
    self,
    protocols::simple_text_output::{self, ProtocolClearScreen, ProtocolOutputString},
};

#[panic_handler]
fn panic_handler(_info: &core::panic::PanicInfo) -> ! {
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

type Result = core::result::Result<(), Error>;

fn res(s: efi::Status) -> Result {
    if s.is_error() {
        Err(s.into())
    } else {
        Ok(())
    }
}

struct Log {
    print: ProtocolOutputString,
    clear: ProtocolClearScreen,
    protocol: *mut simple_text_output::Protocol,
}

impl Log {
    fn new(st: &mut efi::SystemTable) -> Log {
        unsafe {
            Log {
                print: (*st.con_out).output_string,
                clear: (*st.con_out).clear_screen,
                protocol: &mut *st.con_out,
            }
        }
    }

    fn clear(&self) -> Result {
        res((self.clear)(self.protocol))
    }

    fn print(&self, str: &str) -> Result {
        let mut status = efi::Status::SUCCESS;

        ucs2::encode_with(str, |ch| {
            let mut buf = [ch, 0];

            status = (self.print)(self.protocol, &mut buf as *mut _);
            if status.is_error() {
                Err(ucs2::Error::BufferOverflow)
            } else {
                Ok(())
            }
        })
        .map_err(|_| Error::Efi(status))
    }
}

impl Write for Log {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.print(s).map_err(|_| fmt::Error)
    }
}

fn start(st: &mut efi::SystemTable) -> Result {
    let mut log = Log::new(st);
    log.clear()?;
    write!(log, "Syzygy EFI loader {}.", env!("CARGO_PKG_VERSION"))?;

    Ok(())
}

#[no_mangle]
pub extern "efiapi" fn efi_main(_h: efi::Handle, st: &mut efi::SystemTable) -> efi::Status {
    match start(st) {
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
        Err(Error::Efi(s)) => s,
        Err(Error::Fmt) => efi::Status::DEVICE_ERROR,
    }
}
