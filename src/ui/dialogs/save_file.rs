use gtk::prelude::*;
use std::path::PathBuf;

pub fn save_file(parent_window: &gtk::Window) -> Option<PathBuf> {
    let dlg = gtk::FileChooserDialogBuilder::new()
        .title("Save file")
        .transient_for(parent_window)
        .action(gtk::FileChooserAction::Save)
        .icon_name("password-storage")
        .window_position(gtk::WindowPosition::CenterOnParent)
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Save", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);
    let save_clicked = dlg.run() == gtk::ResponseType::Ok;
    let filename = dlg.get_filename();
    dlg.hide();

    if save_clicked {
        filename
    } else {
        None
    }
}
