use crate::model::tree::RecordNode;
use gtk::{glib, glib::subclass::prelude::*, prelude::*};

mod imp {
    use super::*;
    use std::cell::OnceCell;

    #[derive(Default, glib::Properties)]
    #[properties(wrapper_type = super::SearchMatch)]
    pub struct SearchMatch {
        #[property(get, construct_only)]
        pub record: OnceCell<RecordNode>,
        pub path: OnceCell<Vec<RecordNode>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for SearchMatch {
        const NAME: &'static str = "PSSearchMatch";
        type Type = super::SearchMatch;
        type ParentType = glib::Object;
    }

    #[glib::derived_properties]
    impl ObjectImpl for SearchMatch {}
}

glib::wrapper! {
    pub struct SearchMatch(ObjectSubclass<imp::SearchMatch>);
}

impl SearchMatch {
    pub fn new(record: &RecordNode, path: &[RecordNode]) -> Self {
        let this: Self = glib::Object::builder().property("record", record).build();
        this.imp().path.set(path.to_vec()).ok().unwrap();
        this
    }

    pub fn path(&self) -> &[RecordNode] {
        self.imp().path.get().unwrap()
    }
}
