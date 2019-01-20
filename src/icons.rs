use lazy_static::lazy_static;
use glib::Bytes;
use gio::{Resource, resources_register};
use crate::error::*;

lazy_static! {
    static ref ICONS_RESOURCE_BUNDLE: Bytes = {
        // https://github.com/gtk-rs/glib/issues/120
        let aligned = &include_bytes!(concat!(env!("OUT_DIR"), "/icons.gresource"))[..];
        Bytes::from(aligned)
    };
}

pub fn load_icons() -> Result<()> {
    let resource = Resource::new_from_data(&ICONS_RESOURCE_BUNDLE)?;
    resources_register(&resource);
    Ok(())
}
