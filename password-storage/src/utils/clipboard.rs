use gdk::Atom;
use gtk::Clipboard;

pub fn get_clipboard() -> Clipboard {
    Clipboard::get(&Atom::intern("CLIPBOARD"))
}
