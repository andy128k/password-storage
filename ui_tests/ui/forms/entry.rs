use password_storage::ui::forms::base::FormWidget;
use password_storage::ui::forms::entry::Text;
use std::cell::RefCell;
use std::rc::Rc;

pub fn test_text() {
    let w = Text::new();
    w.get_widget(); // ensure get_widget doesn't panic
}

pub fn test_text_value() {
    let w = Text::new();
    assert_eq!(w.get_value(), None);

    let new_value = "new value".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(w.get_value(), Some(new_value));

    w.set_value(None);
    assert_eq!(w.get_value(), None);
}

pub fn test_text_event() {
    let value = Rc::new(RefCell::new(None));

    let mut w = Text::new();
    let value2 = value.clone();
    w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.cloned()));

    let new_value = "new value".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(*value.borrow(), Some(new_value));
}

pub fn test_name() {
    let w = Text::new().with_completion(&[]);
    w.get_widget(); // ensure get_widget doesn't panic
}

pub fn test_name_value() {
    let w = Text::new().with_completion(&[]);
    assert_eq!(w.get_value(), None);

    let new_value = "new value".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(w.get_value(), Some(new_value));

    w.set_value(None);
    assert_eq!(w.get_value(), None);
}

pub fn test_name_event() {
    let value = Rc::new(RefCell::new(None));

    let mut w = Text::new().with_completion(&[]);
    let value2 = value.clone();
    w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.cloned()));

    let new_value = "new value".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(*value.borrow(), Some(new_value));
}

pub fn test_password() {
    let w = Text::new().for_password();
    w.get_widget(); // ensure get_widget doesn't panic
}

pub fn test_password_value() {
    let w = Text::new().for_password();
    assert_eq!(w.get_value(), None);

    let new_value = "secret".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(w.get_value(), Some(new_value));

    w.set_value(None);
    assert_eq!(w.get_value(), None);
}

pub fn test_password_event() {
    let value = Rc::new(RefCell::new(None));

    let mut w = Text::new().for_password();
    let value2 = value.clone();
    w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.cloned()));

    let new_value = "secret".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(*value.borrow(), Some(new_value));
}
