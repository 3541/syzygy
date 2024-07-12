use log::info;

use crate::boot::kmain;

#[no_mangle]
extern "C" fn kinit() {
    info!("Syzygy kernel {}.", env!("SZ_VER"));

    crate::io::log::init();
    kmain();
}
