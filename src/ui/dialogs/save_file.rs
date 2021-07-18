use crate::utils::promise::Promise;
use gtk::prelude::*;
use std::path::PathBuf;

pub async fn save_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    let dlg = gtk::FileChooserDialog::builder()
        .title("Save file")
        .modal(true)
        .transient_for(parent_window)
        .action(gtk::FileChooserAction::Save)
        .icon_name("password-storage")
        .window_position(gtk::WindowPosition::CenterOnParent)
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Save", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    let (promise, future) = Promise::new();
    dlg.connect_response(move |dlg, answer| {
        dlg.hide();
        if answer == gtk::ResponseType::Ok {
            promise.fulfill(dlg.filename());
        } else {
            promise.fulfill(None);
        }
    });
    dlg.show_all();
    future.await.unwrap_or(None)
}
