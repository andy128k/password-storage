use crate::gtk_prelude::*;

pub fn get_clipboard() -> gtk::Clipboard {
    gtk::Clipboard::get(&gdk::Atom::intern("CLIPBOARD"))
}
