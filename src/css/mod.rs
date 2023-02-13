use crate::gtk_prelude::*;
use crate::utils::style::load_css_from_data;

pub fn load_css(display: &gdk::Display) {
    gtk::StyleContext::add_provider_for_display(
        display,
        &load_css_from_data(include_str!("application.css")),
        gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
    );
    gtk::StyleContext::add_provider_for_display(
        display,
        &load_css_from_data(include_str!("fallback.css")),
        gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
    );
}
