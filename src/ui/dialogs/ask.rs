use gtk::prelude::*;

pub fn ask(parent_window: &gtk::Window, message: &str) -> bool {
    let dlg = gtk::MessageDialog::builder()
        .transient_for(parent_window)
        .window_position(gtk::WindowPosition::CenterOnParent)
        .title("Password Storage")
        .icon_name("password-storage")
        .message_type(gtk::MessageType::Warning)
        .buttons(gtk::ButtonsType::YesNo)
        .use_markup(false)
        .text(message)
        .build();
    dlg.set_default_response(gtk::ResponseType::Yes);

    let answer = dlg.run();
    dlg.hide();

    answer == gtk::ResponseType::Yes
}
