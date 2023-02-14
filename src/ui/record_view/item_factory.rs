use super::item::PSRecordViewItem;
use crate::model::tree::RecordNode;
use crate::weak_map::WeakMap;
use gtk::{gio, glib, prelude::*};
use std::cell::RefCell;
use std::rc::Rc;

fn get_record_item(list_item: &gtk::ListItem) -> Option<PSRecordViewItem> {
    list_item.child().and_downcast::<PSRecordViewItem>()
}

fn get_record_node(list_item: &gtk::ListItem) -> Option<RecordNode> {
    list_item.item().and_downcast::<RecordNode>()
}

pub fn item_factory(
    popup_model: Rc<RefCell<Option<gio::MenuModel>>>,
    mapping: &Rc<WeakMap<u32, PSRecordViewItem>>,
) -> gtk::ListItemFactory {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(move |_factory, list_item| {
        let child = PSRecordViewItem::new();
        let popup_model = popup_model.clone();
        child.connect_context_menu(move |_record| popup_model.borrow().clone());
        list_item.set_child(Some(&child));
    });
    factory.connect_bind(glib::clone!(@strong mapping => move |_factory, list_item| {
        let Some(record_item) = get_record_item(list_item) else { return };
        let Some(record_node) = get_record_node(list_item) else { return };
        mapping.remove_value(&record_item);
        mapping.add(list_item.position(), &record_item);
        record_item.set_record_node(Some(record_node));
    }));
    factory.connect_unbind(glib::clone!(@strong mapping => move |_factory, list_item| {
        let Some(record_item) = get_record_item(list_item) else { return };
        mapping.remove_key(list_item.position());
        mapping.remove_value(&record_item);
        record_item.set_record_node(None);
    }));
    factory.connect_teardown(glib::clone!(@strong mapping => move |_factory, list_item| {
        if let Some(record_item) = get_record_item(list_item) {
            record_item.set_record_node(None);
            mapping.remove_value(&record_item);
        }
        list_item.set_child(gtk::Widget::NONE);
    }));
    factory.upcast()
}
