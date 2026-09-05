pub fn init() {
    cfg_select! {
        target_os = "macos" => {
            use gtk::prelude::*;

            if let Some(settings) = gtk::Settings::default() {
                settings.set_property("gtk-decoration-layout", "close,minimize,maximize");
            } else {
                eprintln!("Failed to configure global settings: No default settings found.");
            }
        }
        _ => {}
    }
}
