use super::item::PSRecordViewItem;
use crate::gtk_prelude::*;
use crate::model::tree::RecordNode;
use crate::weak_map::WeakMap;
use std::cell::RefCell;
use std::rc::Rc;

fn get_record_item(list_item: &gtk::ListItem) -> Option<PSRecordViewItem> {
    list_item.child()?.downcast::<PSRecordViewItem>().ok()
}

fn get_record_node(list_item: &gtk::ListItem) -> Option<RecordNode> {
    list_item.item()?.downcast::<RecordNode>().ok()
}

pub fn item_factory(
    popup_model: Rc<RefCell<Option<gio::MenuModel>>>,
    mapping: &Rc<WeakMap<RecordNode, PSRecordViewItem>>,
) -> gtk::ListItemFactory {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(move |_factory, list_item| {
        let child = PSRecordViewItem::new();
        let popup_model = popup_model.clone();
        child.connect_context_menu(move |_record| popup_model.borrow().clone());
        list_item.set_child(Some(&child));
    });
    factory.connect_bind(clone!(@strong mapping => move |_factory, list_item| {
        let Some(record_item) = get_record_item(list_item) else { return };
        let Some(record_node) = get_record_node(list_item) else { return };
        mapping.remove_value(&record_item);
        mapping.add(&record_node, &record_item);
        record_item.set_record_node(Some(record_node));
    }));
    factory.connect_unbind(clone!(@strong mapping => move |_factory, list_item| {
        let Some(record_item) = get_record_item(list_item) else { return };
        let Some(record_node) = get_record_node(list_item) else { return };
        record_item.set_record_node(None);
        mapping.remove_key(&record_node);
        mapping.remove_value(&record_item);
    }));
    factory.connect_teardown(clone!(@strong mapping => move |_factory, list_item| {
        if let Some(record_item) = get_record_item(list_item) {
            record_item.set_record_node(None);
            mapping.remove_value(&record_item);
        }
        list_item.set_child(gtk::Widget::NONE);
    }));
    factory.upcast()
}
