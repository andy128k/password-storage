use super::forms::base::FormWidget;
use crate::utils::promise::Promise;
use gtk::{
    gdk,
    glib::{self, clone},
    prelude::*,
};

pub async fn edit_object<T: 'static, W: FormWidget<T> + 'static>(
    object: Option<&T>,
    mut widget: W,
    parent_window: &gtk::Window,
    title: &str,
    icon: &str,
) -> Option<T> {
    let dlg = gtk::Dialog::builder()
        .type_hint(gdk::WindowTypeHint::Dialog)
        .gravity(gdk::Gravity::Center)
        .transient_for(parent_window)
        .window_position(gtk::WindowPosition::CenterOnParent)
        .use_header_bar(1)
        .modal(true)
        .resizable(true)
        .skip_taskbar_hint(true)
        .skip_pager_hint(true)
        .title(title)
        .icon_name(icon)
        .border_width(8)
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Ok", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    dlg.content_area()
        .pack_start(&widget.get_widget(), true, true, 8);
    dlg.show_all();

    dlg.set_response_sensitive(gtk::ResponseType::Ok, widget.get_value().is_some());
    widget.connect_changed(Box::new(clone!(@weak dlg => move |value| {
        dlg.set_response_sensitive(gtk::ResponseType::Ok, value.is_some());
    })));

    widget.set_value(object);

    let (promise, future) = Promise::<Option<T>>::new();
    dlg.connect_response(move |dlg, answer| {
        dlg.close();
        if answer == gtk::ResponseType::Ok {
            let new_object = widget.get_value();
            promise.fulfill(new_object);
        } else {
            promise.fulfill(None);
        }
    });
    dlg.show_all();
    future.await.unwrap_or(None)
}
