use crate::gtk_prelude::*;

pub fn bold(value: &str) -> String {
    format!("<b>{}</b>", glib::markup_escape_text(value))
}
