use crate::gtk_prelude::*;
use crate::utils::run_once::RunOnce;
use lazy_static::lazy_static;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub fn load_css_from_data(data: &[u8]) -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(data);
    provider
}

pub fn load_static_css<W: IsA<gtk::Widget>>(widget: &W, data: &'static [u8]) {
    lazy_static! {
        static ref INITIALIZED_CSS: RunOnce<(u64, String)> = Default::default();
    }

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
