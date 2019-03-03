use gtk::{TreeModel, TreeModelExt, TreeIter};

pub struct TreeChildrenIter {
    model: TreeModel,
    iter: Option<TreeIter>
}

impl Iterator for TreeChildrenIter {
    type Item = TreeIter;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.iter.clone();

        if let Some(iter) = self.iter.take() {
            if self.model.iter_next(&iter) {
                self.iter = Some(iter);
            } else {
                self.iter = None;
            }
        }
        result
    }
}

pub fn tree_children_entries(model: &TreeModel, parent: Option<&TreeIter>) -> TreeChildrenIter {
    TreeChildrenIter {
        model: model.clone(),
        iter: model.iter_children(parent)
    }
}

pub struct TreeParentsIter {
    model: TreeModel,
    iter: Option<TreeIter>
}

impl Iterator for TreeParentsIter {
    type Item = TreeIter;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.iter.clone();

        if let Some(iter) = self.iter.take() {
            self.iter = self.model.iter_parent(&iter);
        }
        result
    }
}

pub fn tree_parents_entries(model: &TreeModel, iter: &TreeIter) -> TreeParentsIter {
    TreeParentsIter {
        model: model.clone(),
        iter: Some(iter.clone())
    }
}
