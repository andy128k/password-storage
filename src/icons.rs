use gio::{resources_register, Resource};
use glib::{error::Error, Bytes};

fn load_icons_resource() -> Result<Resource, Error> {
    #[cfg(not(target_os = "windows"))]
    const ICONS_RESOURCE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/icons.gresource"));

    #[cfg(target_os = "windows")]
    const ICONS_RESOURCE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "\\icons.gresource"));

    let bytes = {
        // https://github.com/gtk-rs/glib/issues/120
        let aligned = &ICONS_RESOURCE[..];
        Bytes::from(aligned)
    };

    Resource::new_from_data(&bytes)
}

pub fn load_icons() -> Result<(), Error> {
    let resource = load_icons_resource()?;
    resources_register(&resource);
    Ok(())
}
