use crate::gtk_prelude::*;
use crate::utils::promise::Promise;
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
        let result = if answer == gtk::ResponseType::Ok {
            dlg.filename()
        } else {
            None
        };
        promise.fulfill(result);
    });
    dlg.show_all();
    future.await.unwrap_or(None)
}
