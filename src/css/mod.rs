use crate::gtk_prelude::*;

fn load_data(data: &[u8]) -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(data).expect("CSS is loaded");
    provider
}

pub fn load_css(screen: &gdk::Screen) {
    gtk::StyleContext::add_provider_for_screen(
        screen,
        &load_data(include_bytes!("application.css")),
        gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
    );
    gtk::StyleContext::add_provider_for_screen(
        screen,
        &load_data(include_bytes!("fallback.css")),
        gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
    );
}
