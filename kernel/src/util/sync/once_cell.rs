use core::ops::Deref;

use super::Transform;

pub struct OnceCell<T>(Transform<(), T>);

impl<T> OnceCell<T> {
    pub const fn new() -> OnceCell<T> {
        OnceCell(Transform::new(()))
    }

    // Initialize the cell. Panics if double-initialized.
    pub fn init(&self, data: T) {
        self.0.transform(data);
    }

    pub fn borrow(&self) -> Option<&T> {
        self.0.as_ref().right()
    }
}

impl<T> Deref for OnceCell<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.borrow()
            .expect("Tried to dereference an uninitialized OnceCell.")
    }
}
