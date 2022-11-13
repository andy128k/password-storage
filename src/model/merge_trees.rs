use crate::utils::typed_list_store::TypedListStore;

use super::record::*;
use super::tree::*;

fn record_exists<P: Fn(&Record) -> bool>(tree: &TypedListStore<RecordNode>, predicate: &P) -> bool {
    for node in tree {
        if predicate(node.record()) {
            return true;
        }
        if let Some(children) = node.children() {
            if record_exists(children, predicate) {
                return true;
            }
        }
    }
    false
}

fn filter_tree<P: Fn(&Record) -> bool>(
    nodes: &TypedListStore<RecordNode>,
    predicate: &P,
) -> TypedListStore<RecordNode> {
    nodes
        .iter()
        .filter(|node| predicate(node.record()))
        .map(|node| {
            if let Some(children) = node.children() {
                RecordNode::group(node.record().clone(), &filter_tree(children, predicate))
            } else {
                node
            }
        })
        .collect()
}

fn merge_subtries(
    tree1: &TypedListStore<RecordNode>,
    tree2: &TypedListStore<RecordNode>,
) -> TypedListStore<RecordNode> {
    let result: TypedListStore<RecordNode> = Default::default();
    for node in tree1 {
        if let Some(children) = node.children() {
            let name = node.record().name();

            if let Some(node2) = tree2.take_if(|n| n.is_group() && n.record().name() == name) {
                let mut group = node.record().clone();
                group.join(node2.record());

                result.append(&RecordNode::group(
                    group,
                    &merge_subtries(children, node2.children().unwrap()),
                ));
            } else {
                result.append(&node);
            }
        } else {
            result.append(&node);
        }
    }
    for node in tree2 {
        result.append(&node);
    }
    result
}

pub fn merge_trees(tree1: &RecordTree, tree2: &RecordTree) -> RecordTree {
    let without_duplicates = filter_tree(&tree2.records, &|record2| {
        !record_exists(&tree1.records, &|record1| record1 == record2)
    });
    RecordTree {
        records: merge_subtries(&tree1.records, &without_duplicates),
    }
}
