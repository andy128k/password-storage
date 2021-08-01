use crate::gtk_prelude::*;

pub fn create_search_entry() -> gtk::Entry {
    let entry = gtk::Entry::builder()
        .primary_icon_name("edit-find")
        .secondary_icon_name("edit-clear")
        .build();

    entry.connect_icon_release(move |entry, pos, _button| {
        if pos == gtk::EntryIconPosition::Secondary {
            entry.set_text("");
        }
    });

    entry
}
