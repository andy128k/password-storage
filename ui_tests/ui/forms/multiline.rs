use password_storage::ui::forms::base::FormWidget;
use password_storage::ui::forms::multiline::MultiLine;
use std::cell::RefCell;
use std::rc::Rc;

pub fn test_multiline() {
    let w = MultiLine::new();
    w.get_widget(); // ensure get_widget doesn't panic
}

pub fn test_multiline_value() {
    let w = MultiLine::new();
    assert_eq!(w.get_value(), None);

    let new_value = "new\nvalue".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(w.get_value(), Some(new_value));

    w.set_value(None);
    assert_eq!(w.get_value(), None);
}

pub fn test_multiline_event() {
    let value = Rc::new(RefCell::new(None));

    let mut w = MultiLine::new();
    let value2 = value.clone();
    w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.cloned()));

    let new_value = "new\nvalue".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(*value.borrow(), Some(new_value));
}
