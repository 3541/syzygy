mod spin;
use ::spin::{Mutex, MutexGuard};

pub use self::spin::{
    RawSpinLock, RwSpinLock, RwSpinLockReadGuard, RwSpinLockWriteGuard, SpinLock, SpinLockGuard,
};

pub struct SpinLocked<T>(Mutex<T>);

impl<T> SpinLocked<T> {
    pub const fn new(inner: T) -> Self {
        SpinLocked(Mutex::new(inner))
    }

    pub fn lock(&self) -> MutexGuard<T> {
        self.0.lock()
    }
}