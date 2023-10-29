use gtk::{glib, prelude::*};
use std::rc::Rc;

pub trait PSListItemFactory: Sized + 'static {
    type Child: IsA<gtk::Widget>;

    fn setup(&self) -> Self::Child;
    fn bind(&self, list_item: &gtk::ListItem, child: &Self::Child);
    fn unbind(&self, _list_item: &gtk::ListItem, _child: &Self::Child) {}
    fn teardown(&self, _list_item: &gtk::ListItem, _child: &Self::Child) {}

    fn into_factory(self) -> gtk::ListItemFactory {
        let this = Rc::new(self);
        let factory = gtk::SignalListItemFactory::new();
        factory.connect_setup(glib::clone!(@strong this => move |_factory, item| {
            if let Some(list_item) = item.downcast_ref::<gtk::ListItem>() {
                let child = this.setup();
                list_item.set_child(Some(&child));
            }
        }));
        factory.connect_bind(glib::clone!(@strong this => move |_factory, item| {
            if let Some(list_item) = item.downcast_ref::<gtk::ListItem>() {
                if let Some(child) = list_item.child().and_downcast::<Self::Child>() {
                    this.bind(list_item, &child);
                };
            }
        }));
        factory.connect_unbind(glib::clone!(@strong this => move |_factory, item| {
            if let Some(list_item) = item.downcast_ref::<gtk::ListItem>() {
                if let Some(child) = list_item.child().and_downcast::<Self::Child>() {
                    this.unbind(list_item, &child);
                }
            }
        }));
        factory.connect_teardown(glib::clone!(@strong this => move |_factory, item| {
            if let Some(list_item) = item.downcast_ref::<gtk::ListItem>() {
                if let Some(child) = list_item.child().and_downcast::<Self::Child>() {
                    this.teardown(list_item, &child);
                }
                list_item.set_child(gtk::Widget::NONE);
            }
        }));
        factory.upcast()
    }
}
