use gtk::prelude::*;
use gdk::RGBA;
use gtk::{StateFlags, Label};

pub fn create_error_label() -> Label {
    let label = Label::new(None);
    label.set_xalign(0.5);
    label.set_yalign(0.5);
    label.set_no_show_all(true);
    label.override_color(StateFlags::empty(), &RGBA { red: 1.0, green: 0.0, blue: 0.0, alpha: 1.0 });
    label
}
