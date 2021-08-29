use crate::gtk_prelude::*;

pub fn load_css(screen: &gdk::Screen) {
    let css = include_bytes!("fallback.css");
    let provider = gtk::CssProvider::new();
    provider.load_from_data(css).expect("CSS is loaded");
    gtk::StyleContext::add_provider_for_screen(
        screen,
        &provider,
        gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
    );
}
