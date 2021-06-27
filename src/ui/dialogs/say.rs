use gtk::prelude::*;

pub fn say_error(parent_window: &gtk::Window, message: &str) {
    let dlg = gtk::MessageDialog::builder()
        .transient_for(parent_window)
        .window_position(gtk::WindowPosition::CenterOnParent)
        .message_type(gtk::MessageType::Error)
        .title("Password Storage")
        .icon_name("password-storage")
        .buttons(gtk::ButtonsType::Ok)
        .use_markup(false)
        .text(message)
        .build();
    dlg.set_default_response(gtk::ResponseType::Ok);
    dlg.run();
    dlg.close();
}

pub fn say_info(parent_window: &gtk::Window, message: &str) {
    let dlg = gtk::MessageDialog::builder()
        .transient_for(parent_window)
        .window_position(gtk::WindowPosition::CenterOnParent)
        .message_type(gtk::MessageType::Info)
        .title("Password Storage")
        .icon_name("password-storage")
        .buttons(gtk::ButtonsType::Ok)
        .use_markup(false)
        .text(message)
        .build();
    dlg.set_default_response(gtk::ResponseType::Ok);
    dlg.run();
    dlg.close();
}
