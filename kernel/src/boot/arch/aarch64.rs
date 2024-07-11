use crate::boot::kmain;

#[no_mangle]
fn kinit() {
    kmain();
}
