//! Single-initialization storage.

use core::ops::Deref;

use super::Transform;

/// A storage location which can be initialized once at runtime. A special case
/// of [Transform](Transform).
pub struct OnceCell<T>(Transform<(), T>);

impl<T> OnceCell<T> {
    /// Create an empty cell.
    pub const fn new() -> OnceCell<T> {
        OnceCell(Transform::new(()))
    }

    /// Initialize the cell. Panics if double-initialized.
    pub fn init(&self, data: T) {
        self.0.transform(data);
    }

    /// Get a reference to the contents, if initialization has already occurred.
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
