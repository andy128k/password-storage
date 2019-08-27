use gtk::prelude::*;
use gtk::{Window, MessageDialog, WindowPosition, ResponseType, DialogFlags, MessageType, ButtonsType};

pub fn say_error(parent_window: &Window, message: &str) {
    let dlg = MessageDialog::new(
        Some(parent_window),
        DialogFlags::empty(),
        MessageType::Error,
        ButtonsType::Ok,
        message
    );
    dlg.set_title("Password Storage");
    dlg.set_icon_name(Some("password-storage"));
    dlg.set_transient_for(Some(parent_window));
    dlg.set_property_use_markup(false);
    dlg.set_property_window_position(WindowPosition::CenterOnParent);
    dlg.set_default_response(ResponseType::Ok);
    dlg.run();
    dlg.destroy();
}

pub fn say_info(parent_window: &Window, message: &str) {
    let dlg = MessageDialog::new(
        Some(parent_window),
        DialogFlags::empty(),
        MessageType::Info,
        ButtonsType::Ok,
        message
    );
    dlg.set_title("Password Storage");
    dlg.set_icon_name(Some("password-storage"));
    dlg.set_transient_for(Some(parent_window));
    dlg.set_property_use_markup(false);
    dlg.set_property_window_position(WindowPosition::CenterOnParent);
    dlg.set_default_response(ResponseType::Ok);
    dlg.run();
    dlg.destroy();
}
