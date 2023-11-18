use super::item::PSRecordViewItem;
use crate::model::tree::RecordNode;
use crate::ui::list_item_factory::PSListItemFactory;
use crate::weak_map::WeakMap;
use gtk::{gio, prelude::*};
use std::cell::RefCell;
use std::rc::Rc;

struct RecordListItemFactory {
    popup_model: Rc<RefCell<Option<gio::MenuModel>>>,
    mapping: Rc<WeakMap<u32, PSRecordViewItem>>,
}

impl PSListItemFactory for RecordListItemFactory {
    type Child = PSRecordViewItem;

    fn setup(&self) -> PSRecordViewItem {
        let child = PSRecordViewItem::default();
        let popup_model = self.popup_model.clone();
        child.connect_context_menu(move |_record| popup_model.borrow().clone());
        child
    }

    fn bind(&self, list_item: &gtk::ListItem, record_item: &PSRecordViewItem) {
        let Some(record_node) = get_record_node(list_item) else {
            return;
        };
        self.mapping.remove_value(record_item);
        self.mapping.add(list_item.position(), record_item);
        record_item.set_record_node(Some(record_node));
    }

    fn unbind(&self, list_item: &gtk::ListItem, record_item: &PSRecordViewItem) {
        self.mapping.remove_key(list_item.position());
        self.mapping.remove_value(record_item);
        record_item.set_record_node(None);
    }

    fn teardown(&self, _list_item: &gtk::ListItem, record_item: &PSRecordViewItem) {
        record_item.set_record_node(None);
        self.mapping.remove_value(record_item);
    }
}

fn get_record_node(list_item: &gtk::ListItem) -> Option<RecordNode> {
    list_item.item().and_downcast::<RecordNode>()
}

pub fn item_factory(
    popup_model: &Rc<RefCell<Option<gio::MenuModel>>>,
    mapping: &Rc<WeakMap<u32, PSRecordViewItem>>,
) -> gtk::ListItemFactory {
    RecordListItemFactory {
        popup_model: popup_model.clone(),
        mapping: mapping.clone(),
    }
    .into_factory()
}
