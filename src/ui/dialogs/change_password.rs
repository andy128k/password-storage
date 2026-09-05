use crate::ui::edit_object::edit_object;
use crate::ui::forms::form::{Form, ValidationResult};
use crate::ui::password_editor::PasswordEditor;

pub async fn change_password(parent_window: &gtk::Window, title: &str) -> Option<String> {
    let form = Form::default();
    form.add("Password", Box::new(password_editor()), true);
    form.add("Confirm password", Box::new(password_editor()), true);
    form.set_validator(Box::new(|values| {
        if values[0] == values[1] {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid("Passwords are not identical".to_string())
        }
    }));
    let mut result = edit_object(None, form, parent_window, title).await?;
    result.pop()
}

fn password_editor() -> PasswordEditor {
    let editor = PasswordEditor::default();
    editor.set_generate(false);
    editor
}
