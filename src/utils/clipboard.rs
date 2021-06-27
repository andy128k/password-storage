use gtk::{gdk::Atom, Clipboard};

pub fn get_clipboard() -> Clipboard {
    Clipboard::get(&Atom::intern("CLIPBOARD"))
}
