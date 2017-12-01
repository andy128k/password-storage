use glib;
use glib_sys;
use std::ffi::CString;

pub fn escape_markup(val: &str) -> String {
    let cval = CString::new(val).unwrap();
    unsafe {
        let escaped_ptr = glib_sys::g_markup_escape_text(cval.as_ptr(), -1);
        return glib::translate::from_glib_full(escaped_ptr);
    }
}

pub fn big(value: &str) -> String {
    format!("<big><b>{}</b></big>", escape_markup(value))
}

pub fn bold(value: &str) -> String {
    format!("<b>{}</b>", escape_markup(value))
}

pub fn url(value: &str) -> String {
    let escaped = escape_markup(value);
    format!("<a href='{}'>{}</a>", escaped, escaped)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape() {
        assert_eq!("".to_string(), escape_markup(""));
        assert_eq!("test".to_string(), escape_markup("test"));
        assert_eq!("&lt;test&gt;".to_string(), escape_markup("<test>"));
        assert_eq!("test &amp; test".to_string(), escape_markup("test & test"));
    }
}
