use super::forms::base::FormWidget;
use crate::gtk_prelude::*;

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
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Ok", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    dlg.content_area().set_margin_start(8);
    dlg.content_area().set_margin_end(8);
    dlg.content_area().set_margin_top(8);
    dlg.content_area().set_margin_bottom(8);
    dlg.content_area()
        .pack_start(&widget.get_widget(), true, true, 0);

    dlg.set_response_sensitive(gtk::ResponseType::Ok, widget.get_value().is_some());
    widget.connect_changed(Box::new(clone!(@weak dlg => move |value| {
        dlg.set_response_sensitive(gtk::ResponseType::Ok, value.is_some());
    })));

    widget.set_value(object);

    dlg.show_all();
    let answer = dlg.run_future().await;
    dlg.close();
    if answer == gtk::ResponseType::Ok {
        widget.get_value()
    } else {
        None
    }
}
