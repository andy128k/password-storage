use super::base::*;
use glib::{Type, Value};
use gtk::prelude::*;
use gtk::{Widget, Entry, EntryIconPosition, EntryCompletion, InputPurpose, ListStore};
use crate::ui::dialogs::ask::ask;
use crate::utils::string::non_empty;
use crate::password::generate_password;

// Common part

pub trait EntryBasedWidget {
    fn entry(&self) -> Entry;
}

fn get_value(entry: &Entry) -> Option<String> {
    entry.get_text().and_then(non_empty)
}

impl<T> FormWidget<String> for T where T: EntryBasedWidget {
    fn get_widget(&self) -> Widget {
        self.entry().clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        get_value(&self.entry())
    }

    fn set_value(&self, value: Option<&String>) {
        match value {
            Some(text) => self.entry().set_text(text),
            None => self.entry().set_text("")
        }
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
    entry: Entry,
}

impl Text {
    pub fn new() -> Self {
        let entry = Entry::new();
        entry.set_can_focus(true);
        entry.set_activates_default(true);
        entry.set_hexpand(true);
        Self { entry }
    }
}

impl EntryBasedWidget for Text {
    fn entry(&self) -> Entry {
        self.entry.clone()
    }
}

// Name

pub struct Name {
    entry: Entry,
}

fn build_completion_model(items: &[String]) -> ListStore {
    let model = ListStore::new(&[Type::String]);
    for item in items {
        let iter = model.append();
        model.set_value(&iter, 0, &Value::from(item));
    }
    model
}

impl Name {
    pub fn new(names: &[String]) -> Self {
        let completion = EntryCompletion::new();
        completion.set_text_column(0);
        completion.set_model(Some(&build_completion_model(names)));
        completion.set_popup_set_width(false);

        let entry = Entry::new();
        entry.set_can_focus(true);
        entry.set_activates_default(true);
        entry.set_completion(Some(&completion));
        entry.set_hexpand(true);
        Name { entry }
    }
}

impl EntryBasedWidget for Name {
    fn entry(&self) -> Entry {
        self.entry.clone()
    }
}

// OpenPassword

pub struct OpenPassword {
    entry: Entry,
}

fn confirm_password_overwrite<P: WidgetExt>(widget: &P) -> bool {
    ask(&widget.get_toplevel().unwrap().downcast().unwrap(), "Do you want to overwrite current password?")
}

impl OpenPassword {
    pub fn new() -> Self {
        let entry = Entry::new();
        entry.set_can_focus(true);
        entry.set_activates_default(true);
        entry.set_icon_from_icon_name(EntryIconPosition::Secondary, Some("system-run"));
        entry.set_icon_tooltip_text(EntryIconPosition::Secondary, Some("Generate password"));

        entry.connect_icon_release(move |e, pos, _button| {
            if pos == EntryIconPosition::Secondary {
                let is_empty = e.get_text().map_or(true, |t| t.is_empty());
                if is_empty || confirm_password_overwrite(e) {
                    let password = generate_password();
                    e.set_text(&password);
                }
            }
        });

        entry.set_hexpand(true);

        OpenPassword { entry }
    }
}

impl EntryBasedWidget for OpenPassword {
    fn entry(&self) -> Entry {
        self.entry.clone()
    }
}

// Password

pub struct Password {
    entry: Entry,
}

impl Password {
    pub fn new() -> Self {
        let entry = Entry::new();
        entry.set_can_focus(true);
        entry.set_activates_default(true);
        entry.set_hexpand(true);
        entry.set_visibility(false);
        entry.set_input_purpose(InputPurpose::Password);
        Self { entry }
    }
}

impl EntryBasedWidget for Password {
    fn entry(&self) -> Entry {
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
    fn test_open_password() {
        test_gtk_init();

        let w = OpenPassword::new();
        w.get_widget(); // ensure get_widget doesn't panic
    }

    #[test]
    fn test_open_password_value() {
        test_gtk_init();

        let w = OpenPassword::new();
        assert_eq!(w.get_value(), None);

        let new_value = "passw0rd".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(w.get_value(), Some(new_value));

        w.set_value(None);
        assert_eq!(w.get_value(), None);
    }

    #[test]
    fn test_open_password_event() {
        test_gtk_init();

        let value = Rc::new(RefCell::new(None));

        let mut w = OpenPassword::new();
        let value2 = value.clone();
        w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.clone().cloned()));

        let new_value = "passw0rd".to_string();
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
