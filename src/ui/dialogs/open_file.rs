use std::path::PathBuf;
use gtk::prelude::*;
use gtk::{Window, WindowPosition, FileChooserDialog, FileChooserAction, ResponseType};

pub fn open_file(parent_window: &Window) -> Option<PathBuf> {
    let dlg = FileChooserDialog::new(
        Some("Open file"),
        Some(parent_window),
        FileChooserAction::Open
    );
    dlg.set_icon_name("password-storage");
    dlg.set_property_window_position(WindowPosition::CenterOnParent);
    dlg.set_transient_for(parent_window);

    dlg.add_button("_Cancel", ResponseType::Cancel.into());
    dlg.add_button("_Open", ResponseType::Ok.into());
    dlg.set_default_response(ResponseType::Ok.into());
    let open_clicked = dlg.run() == ResponseType::Ok.into();
    let filename = dlg.get_filename();
    dlg.destroy();

    if open_clicked {
        filename
    } else {
        None
    }
}
