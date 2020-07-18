use super::forms::base::FormWidget;
use glib::clone;
use gtk::prelude::*;

pub fn edit_object<T, W: FormWidget<T>>(
    object: Option<&T>,
    mut widget: W,
    parent_window: &gtk::Window,
    title: &str,
    icon: &str,
) -> Option<T> {
    let dlg = gtk::DialogBuilder::new()
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

    dlg.get_content_area()
        .pack_start(&widget.get_widget(), true, true, 8);
    dlg.show_all();

    dlg.set_response_sensitive(gtk::ResponseType::Ok, widget.get_value().is_some());
    widget.connect_changed(Box::new(clone!(@weak dlg => move |value| {
        dlg.set_response_sensitive(gtk::ResponseType::Ok, value.is_some());
    })));

    widget.set_value(object);

    let ok = dlg.run() == gtk::ResponseType::Ok;

    if ok {
        let new_object = widget.get_value();
        dlg.close();
        new_object
    } else {
        dlg.close();
        None
    }
}
