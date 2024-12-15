use r_efi::efi::BootServices;

use crate::{load::Image, log::Log, Result};

pub fn map_image(log: &mut Log, bs: &BootServices, image: &Image) -> Result<()> {
    todo!()
}
