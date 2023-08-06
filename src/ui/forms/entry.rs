use super::base::*;
use crate::utils::string::StringExt;
use gtk::{glib, prelude::*};

pub fn form_entry() -> gtk::Entry {
    gtk::Entry::builder()
        .can_focus(true)
        .activates_default(true)
        .hexpand(true)
        .build()
}

pub fn form_entry_with_completion(items: &[String]) -> gtk::Entry {
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

    gtk::Entry::builder()
        .can_focus(true)
        .activates_default(true)
        .hexpand(true)
        .completion(&completion)
        .build()
}

pub fn form_password_entry() -> gtk::Entry {
    gtk::Entry::builder()
        .can_focus(true)
        .activates_default(true)
        .hexpand(true)
        .visibility(false)
        .input_purpose(gtk::InputPurpose::Password)
        .build()
}

impl FormWidget<String> for gtk::Entry {
    fn get_widget(&self) -> gtk::Widget {
        self.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        get_value(&self)
    }

    fn set_value(&self, value: Option<&String>) {
        self.set_text(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        gtk::prelude::EditableExt::connect_changed(self, move |entry| {
            let value = get_value(entry);
            callback(value.as_ref());
        });
    }
}

fn get_value(entry: &gtk::Entry) -> Option<String> {
    entry.text().non_empty().map(|gs| gs.to_string())
}
