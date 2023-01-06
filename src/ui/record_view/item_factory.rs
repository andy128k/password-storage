use super::item::PSRecordViewItem;
use crate::gtk_prelude::*;
use crate::model::tree::RecordNode;
use std::cell::RefCell;
use std::rc::Rc;

fn get_record_item(list_item: &gtk::ListItem) -> Option<PSRecordViewItem> {
    list_item.child()?.downcast::<PSRecordViewItem>().ok()
}

fn get_record_node(list_item: &gtk::ListItem) -> Option<RecordNode> {
    list_item.item()?.downcast::<RecordNode>().ok()
}

pub fn item_factory(popup_model: Rc<RefCell<Option<gio::MenuModel>>>) -> gtk::ListItemFactory {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(move |_factory, list_item| {
        let child = PSRecordViewItem::new();
        let popup_model = popup_model.clone();
        child.connect_context_menu(move |_record| popup_model.borrow().clone());
        list_item.set_child(Some(&child));
    });
    factory.connect_bind(|_factory, list_item| {
        let Some(record_item) = get_record_item(list_item) else { return };
        let record_node = get_record_node(list_item);
        record_item.set_record_node(record_node);
    });
    factory.connect_unbind(|_factory, list_item| {
        if let Some(record_item) = get_record_item(list_item) {
            record_item.set_record_node(None);
        }
    });
    factory.connect_teardown(|_factory, list_item| {
        if let Some(record_item) = get_record_item(list_item) {
            record_item.set_record_node(None);
        }
    });
    factory.upcast()
}
