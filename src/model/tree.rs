use crate::model::record::Record;
use crate::utils::typed_list_store::TypedListStore;
use gtk::glib::{self, subclass::prelude::*};

mod imp {
    use super::*;
    use std::cell::OnceCell;

    #[derive(Default)]
    pub struct RecordNode {
        pub record: OnceCell<Record>,
        pub children: OnceCell<TypedListStore<super::RecordNode>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for RecordNode {
        const NAME: &'static str = "PSRecordNode";
        type Type = super::RecordNode;
    }

    impl ObjectImpl for RecordNode {}
}

glib::wrapper! {
    pub struct RecordNode(ObjectSubclass<imp::RecordNode>);
}

impl RecordNode {
    pub fn leaf(record: Record) -> Self {
        let this: Self = glib::Object::builder().build();
        this.imp().record.set(record).unwrap();
        this
    }

    pub fn group(record: Record, children: &TypedListStore<RecordNode>) -> Self {
        let this: Self = glib::Object::builder().build();
        this.imp().record.set(record).unwrap();
        this.imp().children.set(children.clone()).unwrap();
        this
    }

    pub fn with_record(&self, record: Record) -> Self {
        let this: Self = glib::Object::builder().build();
        this.imp().record.set(record).unwrap();
        if let Some(children) = self.imp().children.get() {
            this.imp().children.set(children.clone()).unwrap();
        }
        this
    }

    pub fn record(&self) -> &Record {
        self.imp().record.get().expect("RecordNode is initialized")
    }

    pub fn children(&self) -> Option<&TypedListStore<RecordNode>> {
        self.imp().children.get()
    }

    pub fn is_group(&self) -> bool {
        self.children().is_some()
    }
}

#[derive(Debug, Clone, Default)]
pub struct RecordTree {
    pub records: TypedListStore<RecordNode>,
}

impl RecordTree {
    pub fn remove(&self, record_node: &RecordNode) {
        fn traverse(list: &TypedListStore<RecordNode>, record_node: &RecordNode) {
            if let Some(position) = list.find(record_node) {
                list.remove(position);
            }
            for item in list {
                if let Some(children) = item.children() {
                    traverse(children, record_node);
                }
            }
        }
        traverse(&self.records, record_node);
    }

    pub fn depth_first_iter(&self, forward: bool) -> impl Iterator<Item = RecordNode> {
        RecordTreeIter {}
    }
}

pub struct RecordTreeIter {
    records: TypedListStore<RecordNode>,
    stack: Vec<TypedListStore<RecordNode>>,
    position: u32,
}

impl Iterator for RecordTreeIter {
    type Item = RecordNode;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
