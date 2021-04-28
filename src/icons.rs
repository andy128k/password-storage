use gio::{resources_register, Resource};
use glib::{error::Error, Bytes};

const ICONS_RESOURCE: &[u8] = include_bytes!(concat!(
    env!("OUT_DIR"),
    env!("PATH_MAIN_SEPARATOR"),
    "icons.gresource"
));

pub fn load_icons() -> Result<(), Error> {
    let bytes = Bytes::from_static(ICONS_RESOURCE);
    let resource = Resource::from_data(&bytes)?;
    resources_register(&resource);
    Ok(())
}
