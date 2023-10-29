use crate::ui::edit_object::edit_object;
use crate::ui::forms::entry::form_password_entry;
use crate::ui::forms::form::{Form, ValidationResult};

pub async fn change_password(parent_window: &gtk::Window) -> Option<String> {
    let mut form = Form::new();
    form.add("Password", Box::new(form_password_entry()), true);
    form.add("Confirm password", Box::new(form_password_entry()), true);
    form.set_validator(Box::new(|values| {
        if values[0] == values[1] {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid("Passwords are not identical".to_string())
        }
    }));
    let result = edit_object(None, form, parent_window, "Change password").await;
    result.map(|values| values[0].clone())
}
