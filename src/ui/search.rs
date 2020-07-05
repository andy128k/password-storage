use crate::utils::string::non_empty;
use glib::Cast;
use gtk::prelude::*;

pub struct PSSearchEntry(gtk::Entry);

impl PSSearchEntry {
    pub fn new() -> Self {
        let entry = gtk::Entry::new();

        entry.set_icon_from_icon_name(gtk::EntryIconPosition::Primary, Some("edit-find"));
        entry.set_icon_from_icon_name(gtk::EntryIconPosition::Secondary, Some("edit-clear"));

        entry.connect_icon_release(move |entry, pos, _button| {
            if pos == gtk::EntryIconPosition::Secondary {
                entry.set_text("");
            }
        });

        PSSearchEntry(entry)
    }

    pub fn connect_changed<F: Fn(Option<String>) + 'static>(&self, changed: F) {
        self.0.connect_changed(move |e| {
            let search_text = non_empty(e.get_text());
            changed(search_text);
        });
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.0.clone().upcast()
    }

    pub fn grab_focus(&self) {
        self.0.grab_focus();
    }

    pub fn set_sensitive(&self, sensitive: bool) {
        self.0.set_sensitive(sensitive);
    }

    pub fn set_text(&self, text: &str) {
        self.0.set_text(text);
    }
}
