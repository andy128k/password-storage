use crate::gtk_prelude::*;

const ICONS_RESOURCE: &[u8] = include_bytes!(concat!(
    env!("OUT_DIR"),
    env!("PATH_MAIN_SEPARATOR"),
    "icons.gresource"
));

pub fn load_icons() -> Result<(), glib::error::Error> {
    let bytes = glib::Bytes::from_static(ICONS_RESOURCE);
    let resource = gio::Resource::from_data(&bytes)?;
    gio::resources_register(&resource);
    Ok(())
}
