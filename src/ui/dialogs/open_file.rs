use gtk::prelude::*;
use std::path::PathBuf;

pub fn open_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    let dlg = gtk::FileChooserDialog::builder()
        .title("Open file")
        .transient_for(parent_window)
        .action(gtk::FileChooserAction::Open)
        .icon_name("password-storage")
        .window_position(gtk::WindowPosition::CenterOnParent)
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Open", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);
    let open_clicked = dlg.run() == gtk::ResponseType::Ok;
    let filename = dlg.filename();
    dlg.hide();

    if open_clicked {
        filename
    } else {
        None
    }
}
