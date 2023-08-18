#[cfg(not(target_os = "macos"))]
pub fn init() {}

#[cfg(target_os = "macos")]
pub fn init() {
    use gtk::prelude::*;

    let Some(settings) = gtk::Settings::default() else {
        eprintln!("Failed to configure global settings: No default settings found.");
        return;
    };
    settings.set_property("gtk-decoration-layout", "close,minimize,maximize");
}
