use glib::Cast;
use gtk::prelude::*;
use gtk::{Widget, Entry, EntryExt, EntryIconPosition, EditableSignals};
use crate::utils::string::non_empty;

pub struct PSSearchEntry(Entry);

impl PSSearchEntry {
    pub fn new() -> Self {
        let entry = Entry::new();

        entry.set_icon_from_icon_name(EntryIconPosition::Primary, Some("edit-find"));
        entry.set_icon_from_icon_name(EntryIconPosition::Secondary, Some("edit-clear"));

        entry.connect_icon_release(move |e, pos, _button| {
            if pos == EntryIconPosition::Secondary {
                e.set_text("");
            }
        });

        PSSearchEntry(entry)
    }

    pub fn connect_changed<F: Fn(Option<String>) + 'static>(&self, changed: F) {
        self.0.connect_changed(move |e| {
            let search_text = e.get_text().and_then(non_empty);
            changed(search_text);
        });
    }

    pub fn get_widget(&self) -> Widget {
        self.0.clone().upcast()
    }

    pub fn grab_focus(&self) {
        self.0.grab_focus();
    }

    pub fn set_sensitive(&self, sensitive: bool) {
        self.0.set_sensitive(sensitive);
    }

    pub fn get_text(&self) -> Option<String> {
        self.0.get_text().and_then(non_empty)
    }
}
