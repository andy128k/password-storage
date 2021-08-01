use crate::error::*;
use crate::gtk_prelude::*;
use crate::ui::error_label::create_error_label;
use crate::utils::promise::Promise;

pub async fn read_file<T, R>(
    parent_window: &gtk::Window,
    read_file_callback: R,
) -> Option<(T, String)>
where
    T: 'static,
    R: Fn(&str) -> Result<T> + 'static,
{
    let dlg = gtk::Dialog::builder()
        .modal(true)
        .transient_for(parent_window)
        .use_header_bar(1)
        .title("Enter password")
        .icon_name("password-storage")
        .resizable(false)
        .type_hint(gdk::WindowTypeHint::Dialog)
        .gravity(gdk::Gravity::Center)
        .skip_taskbar_hint(true)
        .skip_pager_hint(true)
        .build();

    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Open", gtk::ResponseType::Accept);
    dlg.set_default_response(gtk::ResponseType::Accept);

    let error_label = create_error_label();

    let label = gtk::Label::builder()
        .label("Password")
        .xalign(0f32)
        .yalign(0.5f32)
        .build();

    let entry = gtk::Entry::builder()
        .can_focus(true)
        .activates_default(true)
        .visibility(false)
        .hexpand(true)
        .build();

    let grid = gtk::Grid::builder()
        .margin_start(8)
        .margin_end(8)
        .margin_top(8)
        .margin_bottom(8)
        .column_spacing(8)
        .row_spacing(8)
        .build();
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
