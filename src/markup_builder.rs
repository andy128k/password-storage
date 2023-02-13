use gtk::glib;

pub fn bold(value: &str) -> String {
    format!("<b>{}</b>", glib::markup_escape_text(value))
}
