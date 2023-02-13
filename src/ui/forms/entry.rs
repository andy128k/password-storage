use super::base::*;
use crate::utils::string::StringExt;
use gtk::{glib, prelude::*};

pub struct Text {
    entry: gtk::Entry,
}

impl Text {
    pub fn new() -> Self {
        let entry = gtk::Entry::builder()
            .can_focus(true)
            .activates_default(true)
            .hexpand(true)
            .build();
        Self { entry }
    }

    pub fn with_completion(self, items: &[String]) -> Self {
        let model = gtk::ListStore::new(&[glib::Type::STRING]);
        for item in items {
            let iter = model.append();
            model.set_value(&iter, 0, &glib::Value::from(item));
        }

        let completion = gtk::EntryCompletion::builder()
            .model(&model)
            .popup_set_width(true)
            .build();
        // workaround for a bug in GTK https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=805110
        completion.set_text_column(0);

        self.entry.set_completion(Some(&completion));
        self
    }

    pub fn for_password(self) -> Self {
        self.entry.set_visibility(false);
        self.entry.set_input_purpose(gtk::InputPurpose::Password);
        self
    }
}

impl FormWidget<String> for Text {
    fn get_widget(&self) -> gtk::Widget {
        self.entry.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        get_value(&self.entry)
    }

    fn set_value(&self, value: Option<&String>) {
        self.entry
            .set_text(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.entry.connect_changed(move |entry| {
            let value = get_value(entry);
            callback(value.as_ref());
        });
    }
}

fn get_value(entry: &gtk::Entry) -> Option<String> {
    entry.text().non_empty().map(|gs| gs.to_string())
}
