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
    dlg.set_icon_name(Some("password-storage"));
    dlg.set_transient_for(Some(parent_window));
    dlg.set_property_use_markup(false);
    dlg.set_property_window_position(WindowPosition::CenterOnParent);

    dlg.add_button("_Discard changes", ResponseType::Reject);
    dlg.add_button("_Cancel", ResponseType::Cancel);
    dlg.add_button("_Save", ResponseType::Ok);
    dlg.set_default_response(ResponseType::Ok);

    let answer = dlg.run();
    dlg.destroy();

    match answer {
        ResponseType::Reject => AskSave::Discard,
        ResponseType::Ok => AskSave::Save,
        _ => AskSave::Cancel
    }
}
