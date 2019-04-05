mod boot;

use crate::println;

pub fn run() {
    println!("TEST HARNESS!!");
    let test = unsafe { get_test_name() };
    println!("TEST: {}", test);
    loop {}
    crate::exit_qemu(0);
}

static mut TEST_NAME: [u8; 100] = [0; 100];
unsafe fn get_test_name<'a>() -> &'a str {
    for i in 0..100 {
        TEST_NAME[i] = 0;
    }
    crate::println!("start?");
    let mut serial = crate::hardware::serial::SERIAL1.lock();
    let mut i = 0;
    loop {
        match serial.recv_byte() {
            Some(b'_') => break,
            Some(b) => {
                crate::vga_println!("{}", b as char);
                TEST_NAME[i] = b;
                i += 1
            }
            None => {}
        }
    }
    core::str::from_utf8(&TEST_NAME).expect("Test specification is invalid UTF-8.")
}
