use alloc::str;
use alloc::vec::Vec;
use core::ops::Deref;

use logc::error;
use spin::{RwLock, RwLockReadGuard};

use crate::{println, Address, VirtualAddress};

pub static SYMBOLS: GlobalSymbolTable = GlobalSymbolTable::new();

pub struct GlobalSymbolTable<'a>(RwLock<Symbols<'a>>);

impl<'a> GlobalSymbolTable<'a> {
    const fn new() -> Self {
        Self(RwLock::new(Symbols::empty()))
    }

    pub fn init(&self, sym_data: &'a [u8]) {
        let mut lock = self.0.write();
        assert_eq!(lock.len(), 0, "Tried to init kernel symbols twice.");
        lock.init(sym_data);
    }

    pub fn get(&self) -> RwLockReadGuard<Symbols> {
        self.0.read()
    }
}

pub struct Symbols<'a>(Vec<(VirtualAddress, &'a str)>);

impl<'a> Symbols<'a> {
    const fn empty() -> Self {
        Self(Vec::new())
    }

    fn init(&mut self, sym_data: &'a [u8]) {
        let mut count = 0;
        /*self.0 = str::from_utf8(sym_data)
            .expect("Kernel symbols contained invalid UTF-8")
            .lines()
            .map(|l| {
                let mut s = l.split(' ');

                let addr_string = s.next().unwrap();
                //                debug!("trying to parse {}", addr_string);

                let addr = VirtualAddress::new(
                    usize::from_str_radix(addr_string, 16)
                        .expect("Unable to parse address as number"),
                );

                if *addr == 0xffffffffffffffff {
                    logc::error!("it happened here");
                }

                count += 1;
                if count == 329 || count == 330 {
                    logc::warn!("asfasdf");
                }

                s.next().unwrap();

                let sym = s.next().unwrap();

                (addr, sym)
            })
        .collect();*/

        self.0 = Vec::with_capacity(2500);
        for line in unsafe { str::from_utf8_unchecked(sym_data) }.lines() {
            let mut fields = line.split(' ');

            let addr = VirtualAddress::new(
                usize::from_str_radix(fields.next().unwrap(), 16)
                    .expect("Unable to parse address as number"),
            );

            // Eat the type
            fields.next().unwrap();

            let sym = fields.next().unwrap();

            self.0.push((addr, sym));
            if self.0[self.0.len() - 1] != (addr, sym) {
                let (f_addr, f_sym) = self.0[self.0.len() - 1];
                error!(
                    "Symbol just pushed is not as it should be. At {}",
                    self.0.len() - 1
                );
            }
        }

        /*        logc::debug!("329: {:?}", self.0[329]);
        logc::debug!("330: {:?}", self.0[330]);*/
    }

    #[allow(dead_code)]
    pub fn dump_all(&self) {
        for (addr, sym) in &self.0 {
            println!("{} -> {}", addr, sym);
        }
    }

    // TODO: Make this a binary search, probably?
    pub fn find(&self, addr: VirtualAddress) -> Option<&'a str> {
        for (i, (a, s)) in self.0.iter().enumerate() {
            if **a == 0xffffffffffffffff {
                error!("invalid symbol at {}", i);
            }
            if *a <= addr {
                return Some(s);
            }
        }

        None
    }
}

impl<'a> Deref for Symbols<'a> {
    type Target = [(VirtualAddress, &'a str)];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
