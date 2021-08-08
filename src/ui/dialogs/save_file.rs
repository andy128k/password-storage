use crate::gtk_prelude::*;
use crate::utils::promise::Promise;
use std::path::PathBuf;

pub async fn save_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    let dlg = gtk::FileChooserNative::builder()
        .title("Save file")
        .modal(true)
        .transient_for(parent_window)
        .action(gtk::FileChooserAction::Save)
        .build();

    let (promise, future) = Promise::new();
    dlg.connect_response(move |dlg, answer| {
        dlg.hide();
        let result = if answer == gtk::ResponseType::Accept {
            dlg.filename()
        } else {
            None
        };
        promise.fulfill(result);
    });
    dlg.show();
    future.await.unwrap_or(None)
}
