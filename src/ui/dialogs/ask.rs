use gtk::prelude::*;
use gtk::{
    ButtonsType, DialogFlags, MessageDialog, MessageType, ResponseType, Window, WindowPosition,
};

pub fn ask(parent_window: &Window, message: &str) -> bool {
    let dlg = MessageDialog::new(
        Some(parent_window),
        DialogFlags::empty(),
        MessageType::Warning,
        ButtonsType::YesNo,
        message,
    );
    dlg.set_title("Password Storage");
    dlg.set_icon_name(Some("password-storage"));
    dlg.set_transient_for(Some(parent_window));
    dlg.set_property_use_markup(false);
    dlg.set_property_window_position(WindowPosition::CenterOnParent);
    dlg.set_default_response(ResponseType::Yes);

    let answer = dlg.run();
    dlg.close();

    answer == ResponseType::Yes
}
