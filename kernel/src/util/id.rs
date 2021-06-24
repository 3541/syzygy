//! Macro for ID newtypes.

macro_rules! make_id_type {
    ($name:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        struct $name(usize);
    };
}

macro_rules! make_id_impl {
    ($name:ident) => {
        impl $name {
            pub fn new() -> Self {
                static NEXT: core::sync::atomic::AtomicUsize =
                    core::sync::atomic::AtomicUsize::new(0);
                Self(NEXT.fetch_add(1, core::sync::atomic::Ordering::Relaxed))
            }
        }
    };
}

macro_rules! make_id {
    (pub $name:ident) => {
        pub make_id_type!($name);
        make_id_impl!($name);
    };

    ($name:ident) => {
        make_id_type!($name);
        make_id_impl!($name);
    };
}
