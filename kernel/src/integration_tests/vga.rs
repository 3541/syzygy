use crate::vga_text::WRITER;

pub fn run() -> Result<(), &'static str> {
    WRITER.lock().clear_screen();
    for _ in 0..2000 {
        crate::vga_print!("a");
    }

    for _ in 0..80 {
        crate::vga_println!("test");
    }

    Ok(())
}
