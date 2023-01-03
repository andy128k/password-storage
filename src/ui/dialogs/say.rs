use crate::gtk_prelude::*;

async fn say(parent_window: &gtk::Window, message_type: gtk::MessageType, message: &str) {
    let dlg = gtk::MessageDialog::builder()
        .modal(true)
        .transient_for(parent_window)
        .message_type(message_type)
        .title("Password Storage")
        .icon_name("password-storage")
        .buttons(gtk::ButtonsType::Ok)
        .use_markup(false)
        .text(message)
        .build();
    dlg.set_default_response(gtk::ResponseType::Ok);
    dlg.run_future().await;
    dlg.close();
}

pub async fn say_error(parent_window: &gtk::Window, message: &str) {
    say(parent_window, gtk::MessageType::Error, message).await
}

pub async fn say_info(parent_window: &gtk::Window, message: &str) {
    say(parent_window, gtk::MessageType::Info, message).await
}
