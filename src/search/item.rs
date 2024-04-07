use crate::model::tree::RecordNode;
use crate::utils::typed_list_store::TypedListStore;
use gtk::{glib, glib::subclass::prelude::*};

mod imp {
    use super::*;
    use std::cell::OnceCell;

    #[derive(Default)]
    pub struct SearchMatch {
        pub record: OnceCell<RecordNode>,
        pub path: OnceCell<TypedListStore<RecordNode>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for SearchMatch {
        const NAME: &'static str = "PSSearchMatch";
        type Type = super::SearchMatch;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for SearchMatch {}
}

glib::wrapper! {
    pub struct SearchMatch(ObjectSubclass<imp::SearchMatch>);
}

impl SearchMatch {
    pub fn new(record: &RecordNode, path: &TypedListStore<RecordNode>) -> Self {
        let this: Self = glib::Object::builder().build();
        this.imp().record.set(record.clone()).ok().unwrap();
        this.imp().path.set(path.clone()).ok().unwrap();
        this
    }

    pub fn record(&self) -> &RecordNode {
        self.imp().record.get().unwrap()
    }

    pub fn path(&self) -> &TypedListStore<RecordNode> {
        self.imp().path.get().unwrap()
    }
}
