use std::convert::Into;
use gdk::{WindowTypeHint, Gravity};
use gtk::prelude::*;
use gtk::{Window, Dialog, WindowPosition, ResponseType};

pub fn make_std_dialog(parent_window: &Window, title: &str, icon: &str) -> Dialog {
    let dlg = Dialog::new();

    dlg.set_icon_name(icon);
    dlg.set_property_window_position(WindowPosition::CenterOnParent);
    dlg.set_transient_for(parent_window);
    dlg.set_title(title);
    dlg.set_modal(true);
    dlg.set_resizable(true);
    dlg.set_border_width(8);
    dlg.set_type_hint(WindowTypeHint::Dialog);
    dlg.set_gravity(Gravity::Center);
    dlg.set_skip_taskbar_hint(true);
    dlg.set_skip_pager_hint(true);

    dlg.add_button("_Cancel", ResponseType::Cancel.into());
    dlg.add_button("_Ok", ResponseType::Ok.into());
    dlg.set_default_response(ResponseType::Ok.into());
    dlg
}
