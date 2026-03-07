/*
 * LOG: UEFI console logging.
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

use core::fmt;

use r_efi::{
    efi,
    protocols::simple_text_output::{self, ProtocolClearScreen, ProtocolOutputString},
};

use crate::{Error, Result, uefi::res};

#[derive(Clone, Copy)]
pub struct Log {
    print: ProtocolOutputString,
    clear: ProtocolClearScreen,
    protocol: *mut simple_text_output::Protocol,
}

pub static mut LOG: Option<Log> = None;

impl Log {
    fn new(st: &mut efi::SystemTable) -> Log {
        unsafe {
            Log {
                print: (*st.con_out).output_string,
                clear: (*st.con_out).clear_screen,
                protocol: st.con_out,
            }
        }
    }

    // SAFETY: Must be called only once.
    pub unsafe fn init(st: &mut efi::SystemTable) -> Self {
        unsafe {
            LOG = Some(Log::new(st));
            LOG.unwrap()
        }
    }

    pub fn clear(&self) -> Result<()> {
        unsafe { res((self.clear)(self.protocol)) }
    }

    fn print_char(&self, ch: u16) -> efi::Status {
        let mut buf = [ch, 0];
        unsafe { (self.print)(self.protocol, &mut buf as *mut _) }
    }

    fn print(&self, str: &str) -> Result<()> {
        let mut lf = [0u16];
        let mut cr = [0u16];
        ucs2::encode("\n", &mut lf).unwrap();
        ucs2::encode("\r", &mut cr).unwrap();
        let lf = lf[0];
        let cr = cr[0];
        assert_ne!(lf, 0);
        assert_ne!(lf, cr);

        let mut status = efi::Status::SUCCESS;
        ucs2::encode_with(str, |ch| {
            if ch == lf {
                status = self.print_char(cr);

                if status.is_error() {
                    return Err(ucs2::Error::BufferOverflow);
                }
            }

            status = self.print_char(ch);
            if status.is_error() {
                Err(ucs2::Error::BufferOverflow)
            } else {
                Ok(())
            }
        })
        .map_err(|_| Error::Efi(status))
    }
}

impl fmt::Write for Log {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.print(s).map_err(|_| fmt::Error)
    }
}
