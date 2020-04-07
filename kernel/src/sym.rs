use alloc::str;
use alloc::vec::Vec;
use core::ops::Deref;

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
        self.0 = str::from_utf8(sym_data)
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

                s.next().unwrap();

                let sym = s.next().unwrap();

                (addr, sym)
            })
            .collect();
    }

    #[allow(dead_code)]
    pub fn dump_all(&self) {
        for (addr, sym) in &self.0 {
            println!("{} -> {}", addr, sym);
        }
    }

    pub fn find(&self, addr: VirtualAddress) -> &'a str {
        logc::debug!("looking for {}", addr);
        for (a, s) in &self.0 {
            //            logc::debug!("comparing {} <= {}", a, addr);
            if **a == 0xffffffffffffffff {
                //                logc::debug!("invalid symbol of name {}", s);
            }
            if *a <= addr {
                return s;
            }
        }

        "NONE"
    }
}

impl<'a> Deref for Symbols<'a> {
    type Target = [(VirtualAddress, &'a str)];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
