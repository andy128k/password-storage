use crate::model::record::Record;
use crate::utils::typed_list_store::TypedListStore;
use gtk::glib::{self, subclass::prelude::*};
use std::ops::ControlFlow;

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
    pub fn find<'t>(&'t self, record_node: &RecordNode) -> Option<RecordTreePlace<'t>> {
        fn traverse<'t>(
            tree: &'t RecordTree,
            parent: Option<RecordNode>,
            list: &TypedListStore<RecordNode>,
            record_node: &RecordNode,
        ) -> ControlFlow<RecordTreePlace<'t>> {
            if let Some(position) = list.find(record_node) {
                return ControlFlow::Break(RecordTreePlace {
                    tree,
                    parent,
                    position,
                });
            }
            for item in list {
                if let Some(children) = item.children() {
                    traverse(tree, Some(item.clone()), children, record_node)?;
                }
            }
            ControlFlow::Continue(())
        }
        traverse(self, None, &self.records, record_node).break_value()
    }

    pub fn closest_group(&self, record_node: &RecordNode) -> Option<RecordNode> {
        if record_node.is_group() {
            Some(record_node.clone())
        } else {
            self.find(record_node)?.parent
        }
    }

    pub fn replace(&self, old: &RecordNode, new: &RecordNode) {
        fn traverse(list: &TypedListStore<RecordNode>, old: &RecordNode, new: &RecordNode) {
            if let Some(position) = list.find(old) {
                list.set(position, new.clone());
            }
            for item in list {
                if let Some(children) = item.children() {
                    traverse(children, old, new);
                }
            }
        }
        traverse(&self.records, old, new);
    }

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
        RecordTreeIter {
            records: self.records.clone(),
            stack: Vec::new(),
            position: 0,
            forward,
        }
    }

    pub fn insert_at(&self, place: &RecordTreePlace, record_node: &RecordNode) {
        match place.parent {
            Some(ref parent) => parent
                .children()
                .unwrap()
                .insert(Some(place.position as usize), record_node),
            None => self
                .records
                .insert(Some(place.position as usize), record_node),
        }
    }

    pub fn insert_after(&self, place: &RecordTreePlace, record_node: &RecordNode) {
        let next_place = place.next_sibling().map(|p| p.position as usize);
        match place.parent {
            Some(ref parent) => parent.children().unwrap().insert(next_place, record_node),
            None => self.records.insert(next_place, record_node),
        }
    }
}

pub struct RecordTreePlace<'t> {
    tree: &'t RecordTree,
    parent: Option<RecordNode>,
    position: u32,
}

impl<'t> RecordTreePlace<'t> {
    pub fn prev_sibling(&self) -> Option<Self> {
        if self.position > 0 {
            Some(Self {
                tree: self.tree,
                parent: self.parent.clone(),
                position: self.position - 1,
            })
        } else {
            None
        }
    }

    pub fn next_sibling(&self) -> Option<Self> {
        let n = match self.parent {
            Some(ref node) => node.children()?.len(),
            None => self.tree.records.len(),
        };
        if self.position + 1 < n {
            Some(Self {
                tree: self.tree,
                parent: self.parent.clone(),
                position: self.position + 1,
            })
        } else {
            None
        }
    }
}

pub struct RecordTreeIter {
    records: TypedListStore<RecordNode>,
    stack: Vec<TypedListStore<RecordNode>>,
    position: u32,
    forward: bool,
}

impl Iterator for RecordTreeIter {
    type Item = RecordNode;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
