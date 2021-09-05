use crate::gtk_prelude::*;
use crate::utils::style::load_css_from_data;

pub fn load_css(screen: &gdk::Screen) {
    gtk::StyleContext::add_provider_for_screen(
        screen,
        &load_css_from_data(include_bytes!("application.css")),
        gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
    );
    gtk::StyleContext::add_provider_for_screen(
        screen,
        &load_css_from_data(include_bytes!("fallback.css")),
        gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
    );
}
