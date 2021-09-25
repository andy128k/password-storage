use crate::gtk_prelude::*;

#[derive(Debug)]
pub enum SearchEvent {
    Change,
    Next,
    Prev,
}

pub fn create_search_bar() -> (gtk::SearchBar, gtk::SearchEntry) {
    let bx = gtk::Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .build();
    bx.style_context().add_class("linked");

    let entry = gtk::SearchEntry::builder().width_request(300).build();
    bx.add(&entry);

    let next = create_button("go-down-symbolic");
    next.connect_clicked(clone!(@weak entry => move |_| {
        entry.emit_next_match();
    }));
    bx.add(&next);

    let prev = create_button("go-up-symbolic");
    prev.connect_clicked(clone!(@weak entry => move |_| {
        entry.emit_previous_match();
    }));
    bx.add(&prev);

    let bar = gtk::SearchBar::builder().show_close_button(true).build();
    bar.add(&bx);
    bar.connect_entry(&entry);

    (bar, entry)
}

fn create_button(icon: &str) -> gtk::Button {
    let btn = gtk::Button::builder()
        .image(&gtk::Image::from_icon_name(Some(icon), gtk::IconSize::Menu))
        .build();
    btn.style_context().add_class("image-button");
    btn
}
