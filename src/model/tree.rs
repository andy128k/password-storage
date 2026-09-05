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
    pub fn find<'t>(&'t self, record_node: &RecordNode) -> Option<RecordTreePlace<'t>> {
        self.depth_first_iter()
            .find(|place| place.record() == *record_node)
    }

    pub fn closest_group(&self, record_node: &RecordNode) -> Option<RecordNode> {
        if record_node.is_group() {
            Some(record_node.clone())
        } else {
            self.find(record_node)?.parents().last().cloned()
        }
    }

    pub fn replace_at<'t>(&'t self, place: &'t RecordTreePlace<'t>, record_node: &RecordNode) {
        debug_assert!(std::ptr::eq(self, place.tree));
        place
            .holding_list()
            .set(place.position, record_node.clone());
    }

    pub fn replace(&self, old: &RecordNode, new: &RecordNode) {
        if let Some(place) = self.find(old) {
            self.replace_at(&place, new);
        }
    }

    pub fn remove_at<'t>(&'t self, place: &'t RecordTreePlace<'t>) {
        debug_assert!(std::ptr::eq(self, place.tree));
        place.holding_list().remove(place.position);
    }

    pub fn remove(&self, record_node: &RecordNode) {
        if let Some(place) = self.find(record_node) {
            self.remove_at(&place);
        }
    }

    pub fn depth_first_iter<'t>(&'t self) -> impl Iterator<Item = RecordTreePlace<'t>> {
        RecordTreeIter {
            tree: self,
            stack: vec![(None, self.records.clone(), 0)],
        }
    }

    pub fn insert_at(&self, place: &RecordTreePlace, record_node: &RecordNode) {
        debug_assert!(std::ptr::eq(self, place.tree));
        place
            .holding_list()
            .insert(Some(place.position as usize), record_node)
    }

    pub fn insert_after(&self, place: &RecordTreePlace, record_node: &RecordNode) {
        debug_assert!(std::ptr::eq(self, place.tree));
        let next_place = place.next_sibling().map(|p| p.position as usize);
        place.holding_list().insert(next_place, record_node)
    }
}

pub struct RecordTreePlace<'t> {
    tree: &'t RecordTree,
    parents: Vec<RecordNode>,
    position: u32,
    path: Vec<u32>,
}

impl<'t> RecordTreePlace<'t> {
    fn holding_list(&self) -> &TypedListStore<RecordNode> {
        match self.parents.last() {
            None => &self.tree.records,
            Some(parent) => parent.children().unwrap(),
        }
    }

    pub fn record(&self) -> RecordNode {
        self.holding_list().get(self.position).unwrap()
    }

    pub fn path(&self) -> &[u32] {
        &self.path
    }

    pub fn parents(&self) -> &[RecordNode] {
        &self.parents
    }

    pub fn prev_sibling(&self) -> Option<Self> {
        if self.position > 0 {
            Some(Self {
                tree: self.tree,
                position: self.position - 1,
                path: self.path.clone(),
                parents: self.parents.clone(),
            })
        } else {
            None
        }
    }

    pub fn next_sibling(&self) -> Option<Self> {
        let n = self.holding_list().len();
        if self.position + 1 < n {
            Some(Self {
                tree: self.tree,
                position: self.position + 1,
                path: self.path.clone(),
                parents: self.parents.clone(),
            })
        } else {
            None
        }
    }
}

pub struct RecordTreeIter<'t> {
    tree: &'t RecordTree,
    stack: Vec<(Option<RecordNode>, TypedListStore<RecordNode>, u32)>,
}

impl<'t> RecordTreeIter<'t> {
    fn advance(&mut self) -> Option<(u32, usize)> {
        loop {
            let depth = self.stack.len();
            let (_, records, iter_position) = self.stack.last_mut()?;
            if let Some(record) = records.get(*iter_position) {
                let record_position = *iter_position;

                *iter_position += 1;

                if let Some(children) = record.children() {
                    self.stack.push((Some(record.clone()), children.clone(), 0));
                }

                break Some((record_position, depth));
            } else {
                self.stack.pop();
            }
        }
    }
}

impl<'t> Iterator for RecordTreeIter<'t> {
    type Item = RecordTreePlace<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        let (record_position, depth) = self.advance()?;
        let path: Vec<u32> = self
            .stack
            .iter()
            .take(depth)
            .map(|(_, _, p)| p - 1)
            .collect();
        let node_path: Vec<RecordNode> = self
            .stack
            .iter()
            .take(depth)
            .filter_map(|(node, _, _)| node.clone())
            .collect();
        Some(RecordTreePlace {
            tree: self.tree,
            position: record_position,
            path,
            parents: node_path,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::model::record::{
        FIELD_DESCRIPTION, FIELD_NAME, FIELD_PASSWORD, Field, RECORD_TYPE_GENERIC,
        RECORD_TYPE_GROUP, RECORD_TYPE_WEBSITE,
    };

    trait RecordBuilder {
        fn set(self, field: &Field, value: &str) -> Self;
    }

    impl RecordBuilder for Record {
        fn set(mut self, field: &Field, value: &str) -> Self {
            self.set_field(field, value);
            self
        }
    }

    fn test_tree() -> RecordTree {
        RecordTree {
            records: vec![
                RecordNode::group(
                    RECORD_TYPE_GROUP
                        .new_record()
                        .set(&FIELD_NAME, "Group 1")
                        .set(&FIELD_DESCRIPTION, "websites & other secrets"),
                    &vec![
                        RecordNode::leaf(
                            RECORD_TYPE_WEBSITE
                                .new_record()
                                .set(&FIELD_NAME, "website 1")
                                .set(&FIELD_PASSWORD, "letmein"),
                        ),
                        RecordNode::leaf(
                            RECORD_TYPE_WEBSITE
                                .new_record()
                                .set(&FIELD_NAME, "website 2")
                                .set(&FIELD_PASSWORD, "secret"),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ),
                RecordNode::group(
                    RECORD_TYPE_GROUP.new_record().set(&FIELD_NAME, "Group 2"),
                    &vec![
                        RecordNode::group(
                            RECORD_TYPE_GROUP
                                .new_record()
                                .set(&FIELD_NAME, r#"Subgroup 1 ("The First")"#),
                            &vec![
                                RecordNode::leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 3"),
                                ),
                                RecordNode::leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 4"),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        ),
                        RecordNode::group(
                            RECORD_TYPE_GROUP
                                .new_record()
                                .set(&FIELD_NAME, "Subgroup 2"),
                            &vec![
                                RecordNode::leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 5"),
                                ),
                                RecordNode::leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 6"),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ),
                RecordNode::leaf(
                    RECORD_TYPE_GENERIC
                        .new_record()
                        .set(&FIELD_NAME, "generic entry"),
                ),
                RecordNode::leaf(
                    RECORD_TYPE_WEBSITE
                        .new_record()
                        .set(&FIELD_NAME, "website 7"),
                ),
                RecordNode::leaf(
                    RECORD_TYPE_WEBSITE
                        .new_record()
                        .set(&FIELD_NAME, "website 8"),
                ),
            ]
            .into_iter()
            .collect(),
        }
    }

    #[test]
    fn test_traverse_tree() {
        let tree = test_tree();

        assert_eq!(
            tree.depth_first_iter()
                .map(|node| (
                    node.record().record().name(),
                    node.path().to_vec(),
                    node.parents()
                        .into_iter()
                        .map(|r| r.record().name())
                        .collect()
                ))
                .collect::<Vec<(String, Vec<u32>, Vec<String>)>>(),
            vec![
                ("Group 1".to_owned(), vec![0], vec![]),
                (
                    "website 1".to_owned(),
                    vec![0, 0],
                    vec!["Group 1".to_owned(),]
                ),
                (
                    "website 2".to_owned(),
                    vec![0, 1],
                    vec!["Group 1".to_owned(),]
                ),
                ("Group 2".to_owned(), vec![1], vec![]),
                (
                    "Subgroup 1 (\"The First\")".to_owned(),
                    vec![1, 0],
                    vec!["Group 2".to_owned(),]
                ),
                (
                    "website 3".to_owned(),
                    vec![1, 0, 0],
                    vec![
                        "Group 2".to_owned(),
                        "Subgroup 1 (\"The First\")".to_owned()
                    ]
                ),
                (
                    "website 4".to_owned(),
                    vec![1, 0, 1],
                    vec![
                        "Group 2".to_owned(),
                        "Subgroup 1 (\"The First\")".to_owned()
                    ]
                ),
                (
                    "Subgroup 2".to_owned(),
                    vec![1, 1],
                    vec!["Group 2".to_owned(),]
                ),
                (
                    "website 5".to_owned(),
                    vec![1, 1, 0],
                    vec!["Group 2".to_owned(), "Subgroup 2".to_owned()]
                ),
                (
                    "website 6".to_owned(),
                    vec![1, 1, 1],
                    vec!["Group 2".to_owned(), "Subgroup 2".to_owned()]
                ),
                ("generic entry".to_owned(), vec![2], vec![]),
                ("website 7".to_owned(), vec![3], vec![]),
                ("website 8".to_owned(), vec![4], vec![]),
            ]
        );
    }
}
