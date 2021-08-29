use crate::gtk_prelude::*;

async fn confirm(parent_window: &gtk::Window, message: &str, default: gtk::ResponseType) -> bool {
    let dlg = gtk::MessageDialog::builder()
        .modal(true)
        .transient_for(parent_window)
        .window_position(gtk::WindowPosition::CenterOnParent)
        .title("Password Storage")
        .icon_name("password-storage")
        .message_type(gtk::MessageType::Warning)
        .buttons(gtk::ButtonsType::YesNo)
        .use_markup(false)
        .text(message)
        .build();
    dlg.set_default_response(default);
    let answer = dlg.run_future().await;
    dlg.close();
    answer == gtk::ResponseType::Yes
}

pub async fn confirm_likely(parent_window: &gtk::Window, message: &str) -> bool {
    confirm(parent_window, message, gtk::ResponseType::Yes).await
}

pub async fn confirm_unlikely(parent_window: &gtk::Window, message: &str) -> bool {
    confirm(parent_window, message, gtk::ResponseType::No).await
}
