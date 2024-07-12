use crate::boot::kmain;

#[no_mangle]
extern "C" fn kinit() {
    kmain();
}
