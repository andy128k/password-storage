use super::forms::base::FormWidget;
use crate::gtk_prelude::*;
use crate::utils::promise::Promise;

pub async fn edit_object<T: 'static, W: FormWidget<T> + 'static>(
    object: Option<&T>,
    mut widget: W,
    parent_window: &gtk::Window,
    title: &str,
    icon: &str,
) -> Option<T> {
    let dlg = gtk::Dialog::builder()
        .transient_for(parent_window)
        .use_header_bar(1)
        .modal(true)
        .resizable(true)
        .title(title)
        .icon_name(icon)
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Ok", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    dlg.content_area().set_margin_start(8);
    dlg.content_area().set_margin_end(8);
    dlg.content_area().set_margin_top(8);
    dlg.content_area().set_margin_bottom(8);
    dlg.content_area().append(&widget.get_widget());

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
    dlg.show();
    future.await.unwrap_or(None)
}
