use crate::gtk_prelude::*;

pub fn create_search_bar() -> (gtk::SearchBar, gtk::SearchEntry) {
    let entry = gtk::SearchEntry::builder().width_request(300).build();

    let bar = gtk::SearchBar::builder().show_close_button(true).build();
    bar.add(&entry);
    bar.connect_entry(&entry);

    (bar, entry)
}
