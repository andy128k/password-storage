use crate::gtk_prelude::*;
use crate::utils::also::Also;

pub fn create_error_label() -> gtk::Label {
    gtk::Label::builder()
        .xalign(0.0)
        .yalign(0.5)
        .build()
        .also(|label| {
            label.style_context().add_class("error");
            label.hide();
        })
}
