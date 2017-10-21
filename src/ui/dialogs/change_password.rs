use gtk::Window;
use ui::form::entry::Password;
use ui::form::form::{Form, ValidationResult};
use ui::edit_object::edit_object;

pub fn change_password(parent_window: &Window) -> Option<String> {
    let mut form = Form::new();
    form.add("Password", Box::new(Password::new()), true);
    form.add("Confirm password", Box::new(Password::new()), true);
    form.set_validator(Box::new(|values| {
        if values[0] == values[1] {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid("Passwords are not identical".to_string())
        }
    }));
    let result = edit_object(None, form, parent_window, "Change password", "password-storage");
    result.map(|values| values[0].clone())
}
