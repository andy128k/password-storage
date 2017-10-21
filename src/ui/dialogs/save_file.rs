use std::path::PathBuf;
use gtk::prelude::*;
use gtk::{Window, WindowPosition, FileChooserDialog, FileChooserAction, ResponseType};

pub fn save_file(parent_window: &Window) -> Option<PathBuf> {
    let dlg = FileChooserDialog::new(
        Some("Save file"),
        Some(parent_window),
        FileChooserAction::Save
    );
    dlg.set_icon_name("password-storage");
    dlg.set_property_window_position(WindowPosition::CenterOnParent);
    dlg.set_transient_for(parent_window);

    dlg.add_button("_Cancel", ResponseType::Cancel.into());
    dlg.add_button("_Save", ResponseType::Ok.into());
    dlg.set_default_response(ResponseType::Ok.into());
    let save_clicked = dlg.run() == ResponseType::Ok.into();
    let filename = dlg.get_filename();
    dlg.destroy();

    if save_clicked {
        filename
    } else {
        None
    }
}
