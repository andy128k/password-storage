use password_storage::ui::forms::base::FormWidget;
use password_storage::ui::forms::entry::Text;
use password_storage::ui::forms::form::Form;
use std::cell::RefCell;
use std::rc::Rc;

fn new_form() -> Form {
    let mut form = Form::new();
    form.add("field #1", Box::new(Text::new()), false);
    form.add("field #2", Box::new(Text::new()), false);
    form
}

pub fn test_form() {
    let w = new_form();
    w.get_widget(); // ensure get_widget doesn't panic
}

pub fn test_form_value() {
    let w = new_form();
    assert_eq!(w.get_value(), Some(vec!["".to_string(), "".to_string()]));

    let new_value = vec!["value 1".to_string(), "value 2".to_string()];
    w.set_value(Some(&new_value));
    assert_eq!(w.get_value(), Some(new_value));

    w.set_value(None);
    assert_eq!(w.get_value(), Some(vec!["".to_string(), "".to_string()]));
}

pub fn test_form_event() {
    let value = Rc::new(RefCell::new(None));

    let mut w = new_form();
    let value2 = value.clone();
    w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.cloned()));

    let new_value = vec!["value 1".to_string(), "value 2".to_string()];
    w.set_value(Some(&new_value));
    assert_eq!(*value.borrow(), Some(new_value));
}
