use std::rc::Rc;
use gdk::{WindowTypeHint, Gravity};
use gtk::prelude::*;
use gtk::{Window, Dialog, ResponseType, Grid, Label, Entry, EditableSignals};
use crate::error::*;
use crate::ui::error_label::create_error_label;

pub fn read_file<T, R>(parent_window: &Window, read_file_callback: R) -> Option<(T, String)>
    where R: Fn(&str) -> Result<T>
{
    let dlg = Rc::new(Dialog::new());
    dlg.set_border_width(8);
    dlg.set_modal(true);
    dlg.set_resizable(false);
    dlg.set_transient_for(Some(parent_window));
    dlg.set_title("Enter password");

    dlg.set_type_hint(WindowTypeHint::Dialog);
    dlg.set_gravity(Gravity::Center);
    dlg.set_skip_taskbar_hint(true);
    dlg.set_skip_pager_hint(true);

    dlg.set_icon_name(Some("password-storage"));

    dlg.add_button("_Cancel", ResponseType::Cancel);
    dlg.add_button("_Open", ResponseType::Accept);
    dlg.set_default_response(ResponseType::Accept);

    let error_label = create_error_label().expect("Error label is created.");

    let label = Label::new(Some("Password"));
    label.set_xalign(0f32);
    label.set_yalign(0.5f32);

    let entry = Entry::new();
    entry.set_can_focus(true);
    entry.set_activates_default(true);
    entry.set_visibility(false);

    let grid = Grid::new();
    grid.set_column_spacing(8);
    grid.set_row_spacing(8);
    grid.attach(&error_label, 0, 0, 2, 1);
    grid.attach(&label, 0, 1, 1, 1);
    grid.attach(&entry, 1, 1, 1, 1);

    dlg.get_content_area().add(&grid);
    dlg.get_content_area().set_spacing(8);

    let dlg1 = dlg.clone();
    entry.connect_changed(move |e| {
        dlg1.set_response_sensitive(ResponseType::Accept, e.get_chars(0, -1).map_or(0, |t| t.len()) > 0);
    });

    dlg.set_response_sensitive(ResponseType::Accept, false);

    dlg.show_all();

    let mut result = None;
    loop {
        let button = dlg.run();

        if button != ResponseType::Accept.into() {
            break;
        }

        let password: String = entry.get_text().map_or_else(String::new, |s| s.to_string());
        match read_file_callback(&password) {
            Ok(document) => {
                result = Some((document, password));
                break;
            },
            Err(e) => {
                error_label.set_visible(true);
                error_label.set_label(&format!("Can't open this file.\n{}", e));
            }
        }
    }

    dlg.destroy();

    result
}
