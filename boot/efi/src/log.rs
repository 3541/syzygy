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

use crate::{uefi::res, Error, Result};

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
        LOG = Some(Log::new(st));
        LOG.unwrap()
    }

    pub fn clear(&self) -> Result<()> {
        res((self.clear)(self.protocol))
    }

    fn print(&self, str: &str) -> Result<()> {
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

impl fmt::Write for Log {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.print(s).map_err(|_| fmt::Error)
    }
}
