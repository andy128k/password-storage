use crate::gtk_prelude::*;

pub struct TreeChildrenIter {
    model: gtk::TreeModel,
    iter: Option<gtk::TreeIter>,
}

impl Iterator for TreeChildrenIter {
    type Item = gtk::TreeIter;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.iter;

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

pub fn tree_children_entries(
    model: &gtk::TreeModel,
    parent: Option<&gtk::TreeIter>,
) -> TreeChildrenIter {
    TreeChildrenIter {
        model: model.clone(),
        iter: model.iter_children(parent),
    }
}

pub fn flatten_tree(model: &gtk::TreeModel) -> Vec<gtk::TreeIter> {
    fn traverse(
        model: &gtk::TreeModel,
        parent_iter: Option<&gtk::TreeIter>,
        iters: &mut Vec<gtk::TreeIter>,
    ) {
        for i in tree_children_entries(model, parent_iter) {
            iters.push(i);
            traverse(model, Some(&i), iters);
        }
    }

    let mut iters = Vec::new();
    traverse(model, None, &mut iters);
    iters
}
