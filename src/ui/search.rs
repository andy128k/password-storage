use crate::gtk_prelude::*;
use crate::slot::Slot;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub enum SearchEventType {
    Change,
    Next,
    Prev,
}

pub struct SearchEvent {
    pub event_type: SearchEventType,
    pub query: glib::GString,
    pub search_in_secrets: bool,
}

pub struct SearchConfig {
    pub search_in_secrets: bool,
}

#[derive(Clone, glib::Downgrade)]
pub struct PSSearchBar {
    bar: gtk::SearchBar,
    entry: gtk::SearchEntry,
    search_in_secrets: gtk::CheckButton,
    pub on_search: Rc<Slot<SearchEvent>>,
    pub on_configure: Rc<Slot<SearchConfig>>,
}

impl PSSearchBar {
    pub fn new() -> Self {
        let bx = gtk::Box::builder()
            .orientation(gtk::Orientation::Horizontal)
            .spacing(10)
            .build();

        let (entry_box, entry) = create_search_entry_box();
        bx.add(&entry_box);

        let search_in_secrets = gtk::CheckButton::builder()
            .label("Search in secrets (passwords)")
            .can_focus(true)
            .build();
        bx.add(&search_in_secrets);

        let bar = gtk::SearchBar::builder().show_close_button(true).build();
        bar.add(&bx);
        bar.connect_entry(&entry);

        let this = Self {
            bar,
            entry: entry.clone(),
            search_in_secrets: search_in_secrets.clone(),
            on_search: Default::default(),
            on_configure: Default::default(),
        };

        entry.connect_search_changed(clone!(@weak this => move |entry| {
            let search_in_secrets = this.search_in_secrets.is_active();
            this.on_search.emit(SearchEvent { event_type: SearchEventType::Change, query: entry.text(), search_in_secrets });
        }));
        entry.connect_next_match(clone!(@weak this => move |entry| {
            let search_in_secrets = this.search_in_secrets.is_active();
            this.on_search.emit(SearchEvent { event_type: SearchEventType::Next, query: entry.text(), search_in_secrets });
        }));
        entry.connect_previous_match(clone!(@weak this => move |entry| {
            let search_in_secrets = this.search_in_secrets.is_active();
            this.on_search.emit(SearchEvent { event_type: SearchEventType::Prev, query: entry.text(), search_in_secrets });
        }));
        search_in_secrets.connect_clicked(clone!(@weak this => move |b| {
            this.on_configure.emit(SearchConfig { search_in_secrets:  b.is_active() });
        }));

        this
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.bar.clone().upcast()
    }

    pub fn set_search_mode(&self, mode: bool) {
        self.bar.set_search_mode(mode);
        if mode {
            self.entry.grab_focus();
        }
    }

    pub fn reset(&self) {
        self.entry.set_text("");
    }

    pub fn configure(&self, search_in_secrets: bool) {
        self.search_in_secrets.set_active(search_in_secrets);
    }
}

fn create_search_entry_box() -> (gtk::Box, gtk::SearchEntry) {
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

    (bx, entry)
}

fn create_button(icon: &str) -> gtk::Button {
    let btn = gtk::Button::builder()
        .image(&gtk::Image::from_icon_name(Some(icon), gtk::IconSize::Menu))
        .build();
    btn.style_context().add_class("image-button");
    btn
}
