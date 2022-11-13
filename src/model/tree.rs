use crate::gtk_prelude::*;
use crate::model::record::Record;
use crate::utils::typed_list_store::TypedListStore;

mod imp {
    use once_cell::sync::OnceCell;

    use super::*;

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

#[derive(Debug, Clone)]
pub struct RecordTree {
    pub records: TypedListStore<RecordNode>,
}
