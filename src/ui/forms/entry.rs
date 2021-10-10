use super::base::*;
use crate::gtk_prelude::*;
use crate::utils::string::StringExt;

// Common part

pub trait EntryBasedWidget {
    fn entry(&self) -> gtk::Entry;
}

fn get_value(entry: &gtk::Entry) -> Option<String> {
    entry.text().non_empty().map(|gs| gs.to_string())
}

impl<T> FormWidget<String> for T
where
    T: EntryBasedWidget,
{
    fn get_widget(&self) -> gtk::Widget {
        self.entry().upcast()
    }

    fn get_value(&self) -> Option<String> {
        get_value(&self.entry())
    }

    fn set_value(&self, value: Option<&String>) {
        self.entry()
            .set_text(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.entry().connect_changed(move |entry| {
            let value = get_value(entry);
            callback(value.as_ref());
        });
    }
}

// Text

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
}

impl EntryBasedWidget for Text {
    fn entry(&self) -> gtk::Entry {
        self.entry.clone()
    }
}

// Name

pub struct Name {
    entry: gtk::Entry,
}

fn build_completion_model(items: &[String]) -> gtk::ListStore {
    let model = gtk::ListStore::new(&[glib::Type::STRING]);
    for item in items {
        let iter = model.append();
        model.set_value(&iter, 0, &glib::Value::from(item));
    }
    model
}

impl Name {
    pub fn new(names: &[String]) -> Self {
        let completion = gtk::EntryCompletion::builder()
            .model(&build_completion_model(names))
            .popup_set_width(true)
            .build();
        // workaround for a bug in GTK https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=805110
        completion.set_text_column(0);

        let entry = gtk::Entry::builder()
            .can_focus(true)
            .activates_default(true)
            .completion(&completion)
            .hexpand(true)
            .build();

        Self { entry }
    }
}

impl EntryBasedWidget for Name {
    fn entry(&self) -> gtk::Entry {
        self.entry.clone()
    }
}

// Password

pub struct Password {
    entry: gtk::Entry,
}

impl Password {
    pub fn new() -> Self {
        let entry = gtk::Entry::builder()
            .can_focus(true)
            .activates_default(true)
            .hexpand(true)
            .visibility(false)
            .input_purpose(gtk::InputPurpose::Password)
            .build();
        Self { entry }
    }
}

impl EntryBasedWidget for Password {
    fn entry(&self) -> gtk::Entry {
        self.entry.clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::test_gtk_init;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_text() {
        test_gtk_init();

        let w = Text::new();
        w.get_widget(); // ensure get_widget doesn't panic
    }

    #[test]
    fn test_text_value() {
        test_gtk_init();

        let w = Text::new();
        assert_eq!(w.get_value(), None);

        let new_value = "new value".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(w.get_value(), Some(new_value));

        w.set_value(None);
        assert_eq!(w.get_value(), None);
    }

    #[test]
    fn test_text_event() {
        test_gtk_init();

        let value = Rc::new(RefCell::new(None));

        let mut w = Text::new();
        let value2 = value.clone();
        w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.clone().cloned()));

        let new_value = "new value".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(*value.borrow(), Some(new_value));
    }

    #[test]
    fn test_name() {
        test_gtk_init();

        let w = Name::new(&[]);
        w.get_widget(); // ensure get_widget doesn't panic
    }

    #[test]
    fn test_name_value() {
        test_gtk_init();

        let w = Name::new(&[]);
        assert_eq!(w.get_value(), None);

        let new_value = "new value".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(w.get_value(), Some(new_value));

        w.set_value(None);
        assert_eq!(w.get_value(), None);
    }

    #[test]
    fn test_name_event() {
        test_gtk_init();

        let value = Rc::new(RefCell::new(None));

        let mut w = Name::new(&[]);
        let value2 = value.clone();
        w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.clone().cloned()));

        let new_value = "new value".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(*value.borrow(), Some(new_value));
    }

    #[test]
    fn test_password() {
        test_gtk_init();

        let w = Password::new();
        w.get_widget(); // ensure get_widget doesn't panic
    }

    #[test]
    fn test_password_value() {
        test_gtk_init();

        let w = Password::new();
        assert_eq!(w.get_value(), None);

        let new_value = "secret".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(w.get_value(), Some(new_value));

        w.set_value(None);
        assert_eq!(w.get_value(), None);
    }

    #[test]
    fn test_password_event() {
        test_gtk_init();

        let value = Rc::new(RefCell::new(None));

        let mut w = Password::new();
        let value2 = value.clone();
        w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.clone().cloned()));

        let new_value = "secret".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(*value.borrow(), Some(new_value));
    }
}
