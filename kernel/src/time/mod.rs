pub mod arch;

use alloc::boxed::Box;
use core::time::Duration;

use spin::Mutex;

pub trait HardwareTimer {
    fn arm(&mut self, duration: usize);
    fn calibration(&self) -> Duration;
}

pub struct Timer {
    calibration: Duration,
    timer: Mutex<Box<dyn HardwareTimer>>,
}

impl Timer {
    pub fn new(timer: Box<dyn HardwareTimer>) -> Timer {
        Timer {
            calibration: timer.calibration(),
            timer: Mutex::new(timer),
        }
    }
}
