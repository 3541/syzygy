//! Boilerplate for singleton types.

macro_rules! singleton {
    ($name:ident, $t:ty, $impl_t:ty) => {
        static $name: crate::util::sync::OnceCell<$t> = crate::util::sync::OnceCell::new();
        impl $impl_t {
            pub fn the() -> &'static Self {
                &*$name
            }
        }
    };

    ($name:ident, Spinlock<$t:ty>) => {
        static $name: crate::util::sync::OnceCell<Spinlock<$t>> =
            crate::util::sync::OnceCell::new();
        impl $t {
            pub fn the() -> crate::util::sync::SpinlockGuard<'static, Self> {
                $name.lock()
            }
        }
    };

    ($name:ident, $t:ty) => {
        singleton!($name, $t, $t);
    };
}
