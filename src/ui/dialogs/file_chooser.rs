use crate::gtk_prelude::*;
use crate::utils::promise::Promise;
use std::path::PathBuf;

async fn choose_file(
    action: gtk::FileChooserAction,
    title: &str,
    parent_window: &gtk::Window,
) -> Option<PathBuf> {
    let dlg = gtk::FileChooserNative::builder()
        .modal(true)
        .action(action)
        .title(title)
        .transient_for(parent_window)
        .build();

    let (promise, future) = Promise::new();
    dlg.connect_response(move |dlg, answer| {
        dlg.hide();
        let result = if answer == gtk::ResponseType::Accept {
            dlg.file().and_then(|f| f.path())
        } else {
            None
        };
        promise.fulfill(result);
    });
    dlg.show();
    future.await.unwrap_or(None)
}

pub async fn open_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    choose_file(gtk::FileChooserAction::Open, "Open file", parent_window).await
}

pub async fn save_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    choose_file(gtk::FileChooserAction::Save, "Save file", parent_window).await
}
