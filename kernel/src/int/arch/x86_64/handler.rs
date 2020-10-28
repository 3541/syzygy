macro_rules! handler_fn {
    ($vec:path => fn $name:ident($stack_frame:ident) $inner:block) => {
        handler_fn!($vec => fn $name($stack_frame, ) {
            $inner;
        });
    };

    ($vec: path => fn $name:ident($stack_frame:ident, $( $param:ident : $t:ty ),*) $inner: block) => {
        pub extern "x86-interrupt" fn $name($stack_frame: &mut crate::int::arch::InterruptStackFrame, $( $param : $t ),*) {
            $inner;
        }
    };
}
