use password_storage::ui::forms::base::FormWidget;
use password_storage::ui::password_editor::PasswordEditor;
use std::cell::RefCell;
use std::rc::Rc;

pub fn test_open_password() {
    let w = PasswordEditor::default();
    w.get_widget(); // ensure get_widget doesn't panic
}

pub fn test_open_password_value() {
    let w = PasswordEditor::default();
    let w: &dyn FormWidget<String> = &w;
    assert_eq!(w.get_value(), None);

    let new_value = "passw0rd".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(w.get_value(), Some(new_value));

    w.set_value(None);
    assert_eq!(w.get_value(), None);
}

pub fn test_open_password_event() {
    let value = Rc::new(RefCell::new(None));

    let mut editor = PasswordEditor::default();
    let value2 = value.clone();
    editor.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.cloned()));
    let w: &dyn FormWidget<String> = &editor;

    let new_value = "passw0rd".to_string();
    w.set_value(Some(&new_value));
    assert_eq!(*value.borrow(), Some(new_value));
}
