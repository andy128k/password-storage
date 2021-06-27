use gtk::prelude::*;

pub struct TreeChildrenIter {
    model: gtk::TreeModel,
    iter: Option<gtk::TreeIter>,
}

impl Iterator for TreeChildrenIter {
    type Item = gtk::TreeIter;

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

pub fn tree_children_entries(
    model: &gtk::TreeModel,
    parent: Option<&gtk::TreeIter>,
) -> TreeChildrenIter {
    TreeChildrenIter {
        model: model.clone(),
        iter: model.iter_children(parent),
    }
}

pub struct TreeParentsIter {
    model: gtk::TreeModel,
    iter: Option<gtk::TreeIter>,
}

impl Iterator for TreeParentsIter {
    type Item = gtk::TreeIter;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.iter.clone();

        if let Some(iter) = self.iter.take() {
            self.iter = self.model.iter_parent(&iter);
        }
        result
    }
}

pub fn tree_parents_entries(model: &gtk::TreeModel, iter: &gtk::TreeIter) -> TreeParentsIter {
    TreeParentsIter {
        model: model.clone(),
        iter: Some(iter.clone()),
    }
}
