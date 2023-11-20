use gtk::{gdk, glib, prelude::*};

pub fn load_css_from_data(data: &str) -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_string(data);
    provider
}

fn add_static_css(display: &gdk::Display, css: &'static str, priority: u32) {
    let key = glib::Quark::from_str(format!("static_css_{:?}", css.as_ptr()));
    if unsafe { display.qdata::<bool>(key) }.is_none() {
        let provider = load_css_from_data(css);
        gtk::style_context_add_provider_for_display(display, &provider, priority);
        unsafe { display.set_qdata::<bool>(key, true) };
    }
}

pub trait StaticCssExt {
    fn add_static_css(&self, css: &'static str, priority: u32);
}

impl<W: glib::IsA<gtk::Widget>> StaticCssExt for W {
    fn add_static_css(&self, css: &'static str, priority: u32) {
        self.connect_realize(move |this| {
            add_static_css(&this.display(), css, priority);
        });
    }
}

pub trait PSStyleContextExt {
    fn set_css_class(&self, class_name: &str, set: bool);
}

impl<O: glib::IsA<gtk::Widget>> PSStyleContextExt for O {
    fn set_css_class(&self, class_name: &str, set: bool) {
        if set {
            self.add_css_class(class_name);
        } else {
            self.remove_css_class(class_name);
        }
    }
}
