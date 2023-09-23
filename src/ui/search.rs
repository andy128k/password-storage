use gtk::{glib, prelude::*, subclass::prelude::*};

#[derive(Debug, Clone, Copy)]
pub enum SearchEventType {
    Change = 0,
    Next = 1,
    Prev = 2,
}

impl From<u8> for SearchEventType {
    fn from(value: u8) -> Self {
        match value {
            1 => Self::Next,
            2 => Self::Prev,
            _ => Self::Change,
        }
    }
}

pub struct SearchEvent {
    pub event_type: SearchEventType,
    pub query: glib::GString,
    pub search_in_secrets: bool,
}

pub struct SearchConfig {
    pub search_in_secrets: bool,
}

mod imp {
    use super::*;
    use std::sync::OnceLock;

    #[derive(Default)]
    pub struct PSSearchBar {
        pub bar: gtk::SearchBar,
        pub entry: gtk::SearchEntry,
        pub search_in_secrets: gtk::CheckButton,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSSearchBar {
        const NAME: &'static str = "PSSearchBar";
        type Type = super::PSSearchBar;
        type ParentType = gtk::Widget;
    }

    impl ObjectImpl for PSSearchBar {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            self.bar.set_show_close_button(true);
            self.bar.set_parent(&*obj);
            self.bar.connect_entry(&self.entry);

            self.entry.set_width_request(300);

            let next = create_button("go-down-symbolic");
            next.connect_clicked(
                glib::clone!(@weak self as this => move |_| this.entry.emit_next_match()),
            );

            let prev = create_button("go-up-symbolic");
            prev.connect_clicked(
                glib::clone!(@weak self as this => move |_| this.entry.emit_previous_match()),
            );

            self.search_in_secrets
                .set_label(Some("Search in secrets (passwords)"));
            self.search_in_secrets.set_can_focus(true);

            let entry_box = {
                let b = gtk::Box::builder()
                    .orientation(gtk::Orientation::Horizontal)
                    .css_classes(["linked"])
                    .build();
                b.append(&self.entry);
                b.append(&next);
                b.append(&prev);
                b
            };

            let bx = {
                let b = gtk::Box::builder()
                    .orientation(gtk::Orientation::Horizontal)
                    .spacing(10)
                    .build();
                b.append(&entry_box);
                b.append(&self.search_in_secrets);
                b
            };

            self.bar.set_child(Some(&bx));

            self.entry.connect_search_changed(glib::clone!(@weak self as this => move |entry| {
                this.obj().emit_search(SearchEventType::Change, entry.text(), this.search_in_secrets.is_active());
            }));
            self.entry
                .connect_next_match(glib::clone!(@weak self as this => move |entry| {
                    this.obj().emit_search(SearchEventType::Next, entry.text(), this.search_in_secrets.is_active());
                }));
            self.entry
                .connect_previous_match(glib::clone!(@weak self as this => move |entry| {
                    this.obj().emit_search(SearchEventType::Prev, entry.text(), this.search_in_secrets.is_active());
                }));
            self.search_in_secrets
                .connect_toggled(glib::clone!(@weak self as this => move |b| {
                    this.obj().emit_configure(SearchConfig { search_in_secrets:  b.is_active() });
                }));
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    glib::subclass::Signal::builder("configure")
                        .param_types([bool::static_type()])
                        .return_type::<()>()
                        .build(),
                    glib::subclass::Signal::builder("search")
                        .param_types([
                            u8::static_type(),
                            glib::GString::static_type(),
                            bool::static_type(),
                        ])
                        .return_type::<()>()
                        .build(),
                ]
            })
        }

        fn dispose(&self) {
            while let Some(child) = self.obj().first_child() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PSSearchBar {}
}

glib::wrapper! {
    pub struct PSSearchBar(ObjectSubclass<imp::PSSearchBar>)
        @extends gtk::Widget;
}

impl Default for PSSearchBar {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl PSSearchBar {
    pub fn set_search_mode(&self, mode: bool) {
        self.imp().bar.set_search_mode(mode);
        if mode {
            self.imp().entry.grab_focus();
        }
    }

    pub fn reset(&self) {
        self.imp().entry.set_text("");
    }

    pub fn configure(&self, search_in_secrets: bool) {
        self.imp().search_in_secrets.set_active(search_in_secrets);
    }

    fn emit_configure(&self, config: SearchConfig) {
        self.emit_by_name::<()>("configure", &[&config.search_in_secrets]);
    }

    pub fn connect_configure<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(SearchConfig) + 'static,
    {
        self.connect_closure(
            "configure",
            false,
            glib::closure_local!(move |_self: &Self, search_in_secrets: bool| {
                (f)(SearchConfig { search_in_secrets })
            }),
        )
    }

    fn emit_search(
        &self,
        event_type: SearchEventType,
        query: glib::GString,
        search_in_secrets: bool,
    ) {
        self.emit_by_name::<()>("search", &[&(event_type as u8), &query, &search_in_secrets]);
    }

    pub fn connect_search<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(SearchEvent) + 'static,
    {
        self.connect_closure(
            "search",
            false,
            glib::closure_local!(move |_self: &Self,
                                       event_type: u8,
                                       query: glib::GString,
                                       search_in_secrets: bool| {
                (f)(SearchEvent {
                    event_type: event_type.into(),
                    query,
                    search_in_secrets,
                })
            }),
        )
    }
}

fn create_button(icon: &str) -> gtk::Button {
    let button = gtk::Button::builder().icon_name(icon).build();
    button.add_css_class("image-button");
    button
}
