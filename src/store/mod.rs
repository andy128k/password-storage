use crate::entropy::*;
use crate::model::record::Record;
use crate::model::tree::{RecordNode, RecordTree};
use glib::subclass::boxed::BoxedType;
use glib::{Type, Value};
use gtk::prelude::*;
use gtk::{TreeIter, TreeModel, TreeStore};

#[derive(Clone)]
pub struct PSStore {
    model: TreeStore,
}

pub enum TreeStoreColumn {
    Record,
    Name,
    TypeIcon,
    Selection,
    SelectionVisible,
    Strength,
}

impl TreeStoreColumn {
    fn into_primitive(self) -> u8 {
        match self {
            TreeStoreColumn::Record => 0,
            TreeStoreColumn::Name => 1,
            TreeStoreColumn::TypeIcon => 2,
            TreeStoreColumn::Selection => 3,
            TreeStoreColumn::SelectionVisible => 5,
            TreeStoreColumn::Strength => 4,
        }
    }
}

impl From<TreeStoreColumn> for u32 {
    fn from(column: TreeStoreColumn) -> u32 {
        u32::from(column.into_primitive())
    }
}

impl From<TreeStoreColumn> for i32 {
    fn from(column: TreeStoreColumn) -> i32 {
        i32::from(column.into_primitive())
    }
}

fn is_selected(model: &TreeModel, iter: &TreeIter) -> bool {
    model
        .get_value(iter, TreeStoreColumn::Selection.into())
        .downcast()
        .ok()
        .and_then(|v| v.get())
        .unwrap_or(false)
}

fn delete_checked(model: &TreeStore, parent: Option<&TreeIter>) {
    if let Some(i) = model.iter_children(parent) {
        loop {
            let cont = if is_selected(&model.clone().upcast(), &i) {
                model.remove(&i)
            } else {
                delete_checked(model, Some(&i));
                model.iter_next(&i)
            };
            if !cont {
                break;
            }
        }
    }
}

impl PSStore {
    pub fn new() -> Self {
        let model = TreeStore::new(&[
            Record::get_type(),
            Type::String,
            Type::String,
            Type::Bool,
            Type::String,
            Type::Bool,
        ]);
        PSStore { model }
    }

    pub fn from_tree_model(model: &TreeModel) -> Option<Self> {
        let model = model.clone().downcast().ok()?;
        Some(Self { model })
    }

    pub fn from_tree(tree: &RecordTree) -> Self {
        fn add_record(node: &RecordNode, store: &PSStore, parent_iter: Option<&TreeIter>) {
            match *node {
                RecordNode::Group(ref record, ref nodes) => {
                    let iter = store.append(parent_iter, record);
                    add_records(nodes, store, Some(&iter));
                }
                RecordNode::Leaf(ref record) => {
                    store.append(parent_iter, record);
                }
            }
        }

        fn add_records(tree: &[RecordNode], store: &PSStore, parent_iter: Option<&TreeIter>) {
            for node in tree {
                add_record(node, store, parent_iter);
            }
        }

        let data = Self::new();
        add_records(&tree.records, &data, None);
        data
    }

    pub fn to_tree(&self) -> RecordTree {
        fn traverse(store: &PSStore, parent_iter: Option<&TreeIter>) -> Vec<RecordNode> {
            let mut records = Vec::new();
            for (i, record) in store.children(parent_iter) {
                if record.record_type.is_group {
                    let children = traverse(store, Some(&i));
                    records.push(RecordNode::Group(record, children));
                } else {
                    records.push(RecordNode::Leaf(record));
                }
            }
            records
        }

        RecordTree {
            records: traverse(self, None),
        }
    }

    pub fn as_model(&self) -> TreeModel {
        self.model.clone().upcast()
    }

    pub fn parents(&self, iter: &TreeIter) -> Vec<(TreeIter, Record)> {
        let model = self.as_model();
        let mut result = Vec::new();
        for i in crate::utils::tree::tree_parents_entries(&model, iter) {
            if let Some(record) = self.get(&i) {
                result.push((i, record));
            }
        }
        result
    }

    pub fn children(&self, iter: Option<&TreeIter>) -> Vec<(TreeIter, Record)> {
        let model = self.as_model();
        let mut result = Vec::new();
        for i in crate::utils::tree::tree_children_entries(&model, iter) {
            if let Some(record) = self.get(&i) {
                result.push((i, record));
            }
        }
        result
    }

    pub fn update(&self, iter: &TreeIter, record: &Record) {
        self.model
            .set_value(iter, TreeStoreColumn::Record.into(), &record.to_value());
        self.model.set_value(
            iter,
            TreeStoreColumn::Name.into(),
            &Value::from(&record.name()),
        );
        self.model.set_value(
            iter,
            TreeStoreColumn::TypeIcon.into(),
            &Value::from(record.record_type.icon),
        );
        if let Some(password) = record.password() {
            let entropy = password_entropy(&AsciiClassifier, password.as_bytes());
            let strength_icon = match entropy.into() {
                PasswordStrenth::VeryWeak => "strength-very-weak",
                PasswordStrenth::Weak => "strength-weak",
                PasswordStrenth::Reasonable => "strength-reasonable",
                PasswordStrenth::Strong => "strength-strong",
                PasswordStrenth::VeryStrong => "strength-very-strong",
            };
            self.model.set_value(
                iter,
                TreeStoreColumn::Strength.into(),
                &Value::from(strength_icon),
            );
        } else {
            self.model
                .set_value(iter, TreeStoreColumn::Strength.into(), &Value::from(""));
        }

        self.model.set_value(
            iter,
            TreeStoreColumn::SelectionVisible.into(),
            &Value::from(&!record.record_type.is_group),
        );
    }

    pub fn append(&self, parent_iter: Option<&TreeIter>, record: &Record) -> TreeIter {
        let iter = self.model.append(parent_iter);
        self.update(&iter, record);
        iter
    }

    pub fn get(&self, iter: &TreeIter) -> Option<Record> {
        self.model
            .get_value(iter, TreeStoreColumn::Record.into())
            .get_some::<&Record>()
            .ok()
            .cloned()
    }

    pub fn delete(&self, iter: &TreeIter) {
        self.model.remove(iter);
    }

    pub fn is_selected(&self, iter: &TreeIter) -> bool {
        is_selected(&self.as_model(), iter)
    }

    pub fn delete_checked(&self) {
        delete_checked(&self.model, None);
    }

    pub fn uncheck_all(&self) {
        fn uncheck(model: &TreeStore, parent: Option<&TreeIter>) {
            for i in crate::utils::tree::tree_children_entries(&model.clone().upcast(), parent) {
                model.set_value(&i, TreeStoreColumn::Selection.into(), &Value::from(&false));
                uncheck(model, Some(&i));
            }
        }
        uncheck(&self.model, None);
    }
}
