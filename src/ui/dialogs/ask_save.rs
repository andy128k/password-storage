use crate::gtk_prelude::*;
use crate::utils::promise::Promise;

pub enum AskSave {
    Discard,
    Cancel,
    Save,
}

pub async fn ask_save(parent_window: &gtk::Window, message: &str) -> AskSave {
    let dlg = gtk::MessageDialog::builder()
        .modal(true)
        .transient_for(parent_window)
        .window_position(gtk::WindowPosition::CenterOnParent)
        .title("Password Storage")
        .icon_name("password-storage")
        .message_type(gtk::MessageType::Warning)
        .use_markup(false)
        .text(message)
        .build();
    dlg.add_button("_Discard changes", gtk::ResponseType::Reject);
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Save", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    let (promise, future) = Promise::<AskSave>::new();
    dlg.connect_response(move |dlg, answer| {
        dlg.close();
        promise.fulfill(match answer {
            gtk::ResponseType::Reject => AskSave::Discard,
            gtk::ResponseType::Ok => AskSave::Save,
            _ => AskSave::Cancel,
        });
    });
    dlg.show_all();
    future.await.unwrap_or(AskSave::Cancel)
}
