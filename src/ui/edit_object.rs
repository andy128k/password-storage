use super::dialogs::std::make_std_dialog;
use super::forms::base::FormWidget;
use gtk::prelude::*;
use gtk::{ResponseType, Window};

pub fn edit_object<T, W: FormWidget<T>>(
    object: Option<&T>,
    mut widget: W,
    parent_window: &Window,
    title: &str,
    icon: &str,
) -> Option<T> {
    let dlg = make_std_dialog(parent_window, title, icon);
    dlg.get_content_area()
        .pack_start(&widget.get_widget(), true, true, 8);
    dlg.show_all();

    dlg.set_response_sensitive(ResponseType::Ok, widget.get_value().is_some());
    {
        let dlg1 = dlg.clone();
        widget.connect_changed(Box::new(move |value| {
            dlg1.set_response_sensitive(ResponseType::Ok, value.is_some());
        }));
    }

    widget.set_value(object);

    let ok = dlg.run() == ResponseType::Ok;

    if ok {
        let new_object = widget.get_value();
        dlg.close();
        new_object
    } else {
        dlg.close();
        None
    }
}
