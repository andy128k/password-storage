use crate::gtk_prelude::*;
use crate::utils::promise::Promise;
use std::path::PathBuf;

pub async fn open_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    let dlg = gtk::FileChooserNative::builder()
        .modal(true)
        .title("Open file")
        .transient_for(parent_window)
        .action(gtk::FileChooserAction::Open)
        .build();

    let (promise, future) = Promise::<Option<PathBuf>>::new();
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
