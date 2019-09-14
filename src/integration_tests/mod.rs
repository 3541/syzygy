mod boot;
mod vga;

use crate::println;

pub fn run() {
    let test = unsafe { get_test_name() };
    println!("TEST: {}", test);
    let res = match test {
        "boot " => boot::run(),
        "vga  " => vga::run(),
        _ => Err("Invalid test specification."),
    };

    let exit_status = if let Ok(()) = res {
        println!("ok");
        0
    } else {
        println!("{}", res.unwrap_err());
        1
    };

    crate::exit_qemu(exit_status);
}

const TEST_NAME_MAX_LEN: usize = 5;
static mut TEST_NAME: [u8; TEST_NAME_MAX_LEN] = [0; TEST_NAME_MAX_LEN];
unsafe fn get_test_name<'a>() -> &'a str {
    for i in 0..TEST_NAME_MAX_LEN {
        TEST_NAME[i] = ' ' as u8;
    }
    let mut serial = crate::hardware::serial::SERIAL1.lock();
    let mut i = 0;
    loop {
        match serial.recv_byte() {
            Some(b'_') => break,
            Some(b) => {
                TEST_NAME[i] = b;
                i += 1
            }
            None => {}
        }
    }
    core::str::from_utf8(&TEST_NAME).expect("Test specification is invalid UTF-8.")
}
