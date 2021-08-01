use crate::gtk_prelude::*;

pub fn big(value: &str) -> String {
    format!("<big><b>{}</b></big>", glib::markup_escape_text(value))
}

pub fn bold(value: &str) -> String {
    format!("<b>{}</b>", glib::markup_escape_text(value))
}

pub fn url(value: &str) -> String {
    let escaped = glib::markup_escape_text(value);
    format!("<a href='{}'>{}</a>", escaped, escaped)
}
