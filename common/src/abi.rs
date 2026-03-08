pub const ENTRYPOINT: &'static str = "kinit";

pub type Entrypoint = unsafe fn() -> !;
