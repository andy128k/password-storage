use crate::gtk_prelude::*;

pub fn load_css_from_data(data: &[u8]) -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(data).expect("CSS is loaded");
    provider
}

#[macro_export]
macro_rules! include_css {
    ($file:expr) => {
        {
            thread_local!(static CSS_PROVIDER: gtk::CssProvider = crate::utils::style::load_css_from_data(include_bytes!($file)));
            CSS_PROVIDER.with(Clone::clone)
        }
    };
}
