use crate::gtk_prelude::*;
use crate::utils::run_once::RunOnce;
use lazy_static::lazy_static;

pub fn load_css_from_data(data: &[u8]) -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(data).expect("CSS is loaded");
    provider
}

pub fn load_static_css<W: IsA<gtk::Widget>>(widget: &W, data: &'static [u8]) {
    lazy_static! {
        static ref INITIALIZED_CSS: RunOnce<(String, String)> = Default::default();
    }

    widget.connect_realize(move |widget| {
        if let Some(screen) = widget.screen() {
            let key = (
                screen.display().name().to_string(),
                widget.type_().name().to_string(),
            );
            INITIALIZED_CSS.run(key, || {
                gtk::StyleContext::add_provider_for_screen(
                    &screen,
                    &load_css_from_data(data),
                    gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
                );
            });
        }
    });
}
