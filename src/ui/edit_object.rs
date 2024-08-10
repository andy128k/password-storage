use super::dialogs::generic_dialog::GenericDialog;
use super::forms::base::FormWidget;
use gtk::{glib, prelude::*};

pub async fn edit_object<T: 'static, W: FormWidget<T> + 'static>(
    object: Option<&T>,
    mut widget: W,
    parent_window: &gtk::Window,
    title: &str,
) -> Option<T> {
    let form = widget.get_widget();
    form.set_hexpand(true);
    form.set_vexpand(true);
    form.set_margin_top(8);
    form.set_margin_bottom(8);
    form.set_margin_start(8);
    form.set_margin_end(8);

    let dlg = GenericDialog::default();
    dlg.set_transient_for(Some(parent_window));
    dlg.set_title(title);
    dlg.set_child(Some(&form));

    dlg.set_ok_sensitive(widget.get_value().is_some());
    widget.connect_changed(Box::new(glib::clone!(
        #[weak]
        dlg,
        move |value| {
            dlg.set_ok_sensitive(value.is_some());
        }
    )));

    widget.set_value(object);

    let answer = dlg.run().await?;
    dlg.close();
    if answer == gtk::ResponseType::Ok {
        widget.get_value()
    } else {
        None
    }
}
