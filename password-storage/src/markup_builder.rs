use glib::markup_escape_text;

pub fn big(value: &str) -> String {
    format!("<big><b>{}</b></big>", markup_escape_text(value))
}

pub fn bold(value: &str) -> String {
    format!("<b>{}</b>", markup_escape_text(value))
}

pub fn url(value: &str) -> String {
    let escaped = markup_escape_text(value);
    format!("<a href='{}'>{}</a>", escaped, escaped)
}
