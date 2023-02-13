use crate::utils::run_once::RunOnce;
use gtk::{glib, prelude::*};
use once_cell::sync::Lazy;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub fn load_css_from_data(data: &str) -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(data);
    provider
}

pub fn load_static_css<W: glib::IsA<gtk::Widget>>(widget: &W, data: &'static str) {
    static INITIALIZED_CSS: Lazy<RunOnce<(u64, String)>> = Lazy::new(RunOnce::default);

    widget.connect_realize(move |widget| {
        let display = widget.display();

        let mut hasher = DefaultHasher::new();
        display.hash(&mut hasher);
        let display_hash = hasher.finish();

        let key = (display_hash, widget.type_().name().to_string());
        INITIALIZED_CSS.run(key, || {
            gtk::StyleContext::add_provider_for_display(
                &display,
                &load_css_from_data(data),
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
        });
    });
}
