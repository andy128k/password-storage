use gtk::prelude::*;
use std::path::PathBuf;

pub async fn open_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    gtk::FileDialog::builder()
        .modal(true)
        .title("Open file")
        .build()
        .open_future(Some(parent_window))
        .await
        .ok()
        .and_then(|f| f.path())
}

pub async fn save_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    gtk::FileDialog::builder()
        .modal(true)
        .title("Save file")
        .build()
        .save_future(Some(parent_window))
        .await
        .ok()
        .and_then(|f| f.path())
}
