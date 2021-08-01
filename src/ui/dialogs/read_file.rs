use crate::error::*;
use crate::ui::error_label::create_error_label;
use crate::utils::promise::Promise;
use gtk::{
    gdk,
    glib::{self, clone},
    prelude::*,
};

pub async fn read_file<T, R>(
    parent_window: &gtk::Window,
    read_file_callback: R,
) -> Option<(T, String)>
where
    T: 'static,
    R: Fn(&str) -> Result<T> + 'static,
{
    let dlg = gtk::Dialog::new();
    dlg.set_border_width(8);
    dlg.set_modal(true);
    dlg.set_resizable(false);
    dlg.set_transient_for(Some(parent_window));
    dlg.set_title("Enter password");

    dlg.set_type_hint(gdk::WindowTypeHint::Dialog);
    dlg.set_gravity(gdk::Gravity::Center);
    dlg.set_skip_taskbar_hint(true);
    dlg.set_skip_pager_hint(true);

    dlg.set_icon_name(Some("password-storage"));

    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Open", gtk::ResponseType::Accept);
    dlg.set_default_response(gtk::ResponseType::Accept);

    let error_label = create_error_label();

    let label = gtk::Label::new(Some("Password"));
    label.set_xalign(0f32);
    label.set_yalign(0.5f32);

    let entry = gtk::Entry::new();
    entry.set_can_focus(true);
    entry.set_activates_default(true);
    entry.set_visibility(false);

    let grid = gtk::Grid::new();
    grid.set_column_spacing(8);
    grid.set_row_spacing(8);
    grid.attach(&error_label, 0, 0, 2, 1);
    grid.attach(&label, 0, 1, 1, 1);
    grid.attach(&entry, 1, 1, 1, 1);

    dlg.content_area().add(&grid);
    dlg.content_area().set_spacing(8);

    entry.connect_changed(clone!(@weak dlg => move |e| {
        dlg.set_response_sensitive(
            gtk::ResponseType::Accept,
            e.chars(0, -1).map_or(0, |t| t.len()) > 0,
        );
    }));

    dlg.set_response_sensitive(gtk::ResponseType::Accept, false);

    dlg.show_all();

    let (promise, future) = Promise::<Option<(T, String)>>::new();
    dlg.connect_response(move |dlg, button| {
        if button != gtk::ResponseType::Accept {
            dlg.close();
            promise.fulfill(None);
        }

        let password = entry.text();
        match read_file_callback(&password) {
            Ok(document) => {
                dlg.close();
                promise.fulfill(Some((document, password.into())));
            }
            Err(e) => {
                error_label.set_visible(true);
                error_label.set_label(&format!("Can't open this file.\n{}", e));
            }
        }
    });
    dlg.show_all();
    future.await.unwrap_or(None)
}
