macro_rules! handler_fn {
    ($vec:path => fn $name:ident($param:ident) $inner:block) => {
        pub extern "x86-interrupt" fn $name($param: &mut crate::int::arch::InterruptStackFrame) {
            $inner;
        }
    };
}
