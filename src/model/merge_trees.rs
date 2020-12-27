use super::record::*;
use super::tree::*;

fn record_exists<P: Fn(&Record) -> bool>(tree: &[RecordNode], predicate: &P) -> bool {
    for node in tree {
        match *node {
            RecordNode::Group(ref record, ref subtree) => {
                if predicate(record) || record_exists(subtree, predicate) {
                    return true;
                }
            }
            RecordNode::Leaf(ref record) => {
                if predicate(record) {
                    return true;
                }
            }
        }
    }
    false
}

fn filter_tree<P: Fn(&Record) -> bool>(tree: &[RecordNode], predicate: &P) -> Vec<RecordNode> {
    let mut nodes = Vec::new();
    for node in tree {
        match *node {
            RecordNode::Group(ref record, ref subtree) => {
                if predicate(record) {
                    nodes.push(RecordNode::Group(
                        record.clone(),
                        filter_tree(subtree, predicate),
                    ));
                }
            }
            RecordNode::Leaf(ref record) => {
                if predicate(record) {
                    nodes.push(RecordNode::Leaf(record.clone()));
                }
            }
        }
    }
    nodes
}

fn merge_subtries(tree1: &mut Vec<RecordNode>, tree2: &[RecordNode]) {
    for node in tree2 {
        match node {
            &RecordNode::Group(ref group, ref subtree) => {
                let mut found = false;
                for t1_node in tree1.iter_mut() {
                    if let RecordNode::Group(ref mut dst_group, ref mut dst_subtree) = *t1_node {
                        if dst_group.name() == group.name() {
                            dst_group.join(group);
                            merge_subtries(dst_subtree, subtree);
                            found = true;
                            break;
                        }
                    }
                }
                if !found {
                    tree1.push(RecordNode::Group(group.clone(), subtree.clone()));
                }
            }
            leaf @ &RecordNode::Leaf(_) => {
                tree1.push(leaf.clone());
            }
        }
    }
}

pub fn merge_trees(tree1: &mut RecordTree, tree2: &RecordTree) {
    let without_duplicates = filter_tree(&tree2.records, &|record2| {
        !record_exists(&tree1.records, &|record1| record1 == record2)
    });
    merge_subtries(&mut tree1.records, &without_duplicates);
}
