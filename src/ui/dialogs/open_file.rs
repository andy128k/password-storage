use crate::gtk_prelude::*;
use crate::utils::promise::Promise;
use std::path::PathBuf;

pub async fn open_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    let dlg = gtk::FileChooserDialog::builder()
        .modal(true)
        .title("Open file")
        .transient_for(parent_window)
        .action(gtk::FileChooserAction::Open)
        .icon_name("password-storage")
        .window_position(gtk::WindowPosition::CenterOnParent)
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Open", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    let (promise, future) = Promise::<Option<PathBuf>>::new();
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
