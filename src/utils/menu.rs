use gtk::prelude::*;
use gtk::{Widget, Menu};
use glib::translate::ToGlibPtr;

pub trait MenuExtManual {
    fn attach_to_widget_no_detacher<P: IsA<Widget>>(&self, attach_widget: &P);
}

impl MenuExtManual for Menu {
    fn attach_to_widget_no_detacher<P: IsA<Widget>>(&self, attach_widget: &P) {
        unsafe {
            gtk_sys::gtk_menu_attach_to_widget(
                self.to_glib_none().0,
                attach_widget.as_ref().to_glib_none().0,
                None
            );
        }
    }
}
