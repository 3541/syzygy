pub mod arch;

use alloc::boxed::Box;
use core::time::Duration;

use crate::sync::SpinLock;

pub trait HardwareTimer {
    fn arm(&mut self, duration: usize);
    fn calibration(&self) -> Duration;
}

pub struct Timer {
    calibration: Duration,
    timer: SpinLock<Box<dyn HardwareTimer>>,
}

impl Timer {
    pub fn new(timer: Box<dyn HardwareTimer>) -> Timer {
        Timer {
            calibration: timer.calibration(),
            timer: SpinLock::new(timer),
        }
    }
}
