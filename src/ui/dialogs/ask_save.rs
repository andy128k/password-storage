use gtk::prelude::*;
use gtk::{Window, MessageDialog, WindowPosition, ResponseType, DialogFlags, MessageType, ButtonsType};

pub enum AskSave {
    Discard,
    Cancel,
    Save
}

pub fn ask_save(parent_window: &Window, message: &str) -> AskSave {
    let dlg = MessageDialog::new(
        Some(parent_window),
        DialogFlags::empty(),
        MessageType::Warning,
        ButtonsType::None,
        message
    );
    dlg.set_title("Password Storage");
    dlg.set_icon_name("password-storage");
    dlg.set_transient_for(parent_window);
    dlg.set_property_use_markup(false);
    dlg.set_property_window_position(WindowPosition::CenterOnParent);

    dlg.add_button("_Discard changes", ResponseType::Reject.into());
    dlg.add_button("_Cancel", ResponseType::Cancel.into());
    dlg.add_button("_Save", ResponseType::Ok.into());
    dlg.set_default_response(ResponseType::Ok.into());

    let answer = dlg.run();
    dlg.destroy();

    if answer == ResponseType::Reject.into() {
        AskSave::Discard
    } else if answer == ResponseType::Ok.into() {
        AskSave::Save
    } else {
        AskSave::Cancel
    }
}
