use super::{InterruptController, InterruptVector};

pub struct Apic;

unsafe impl InterruptController for Apic {
    fn end_of_interrupt(&mut self, _interrupt: InterruptVector) {
        todo!()
    }

    fn disable(&mut self) {
        todo!()
    }
}
