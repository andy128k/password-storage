use std::rc::Rc;
use std::cell::RefCell;
use gtk::prelude::*;
use gtk::{TreeModel, TreeModelFilter, TreeModelFilterExt, TreeIter};
use crate::model::record::Record;
use crate::store::PSStore;

pub struct PSTreeFilterPrivate {
    model: Option<PSStore>,
    filter: Option<TreeModelFilter>,
    filter_func: Option<Box<dyn Fn(&Record) -> bool>>,
}

#[derive(Clone)]
pub struct PSTreeFilter(Rc<RefCell<PSTreeFilterPrivate>>);

fn record_satisfies(store: &PSStore, filter_func: &(dyn Fn(&Record) -> bool + 'static), iter: &TreeIter) -> bool {
    if let Some(record) = store.get(iter) {
        if filter_func(&record) {
            return true;
        }
    }

    for (i, _sub_record) in store.children(Some(iter)) {
        if record_satisfies(store, filter_func, &i) {
            return true;
        }
    }

    false
}

fn tree_filter_func(private: &PSTreeFilterPrivate, iter: &TreeIter) -> bool {
    match (&private.model, &private.filter_func) {
        (&Some(ref store), &Some(ref filter_func)) => {
            if record_satisfies(store, filter_func.as_ref(), iter) {
                return true;
            }

            for (_i, sup_record) in store.parents(iter) {
                if filter_func(&sup_record) {
                    return true
                }
            }
            false            
        },
        _ => false
    }
}

impl PSTreeFilter {
    pub fn new() -> Self {
        let private = PSTreeFilterPrivate { model: None, filter: None, filter_func: None };
        PSTreeFilter(Rc::new(RefCell::new(private)))
    }

    pub fn as_model(&self) -> Option<TreeModel> {
        self.0.borrow().filter.clone().map(|m| m.upcast())
    }

    pub fn set_model(&self, model: Option<&PSStore>) {
        match model {
            Some(model) => {
                let weak_private = Rc::downgrade(&self.0);
                let filter = TreeModelFilter::new(&model.as_model(), None);
                filter.set_visible_func(move |_model, iter| {
                    if let Some(private) = weak_private.upgrade() {
                        tree_filter_func(&private.borrow(), iter)
                    } else {
                        true
                    }
                });
                self.0.borrow_mut().model = Some(model.clone());
                self.0.borrow_mut().filter = Some(filter);
            },
            None => {
                self.0.borrow_mut().model = None;
                self.0.borrow_mut().filter = None;
            }
        };
    }

    pub fn set_filter_func(&self, filter_func: Option<Box<dyn Fn(&Record) -> bool>>) {
        self.0.borrow_mut().filter_func = filter_func;
    }

    pub fn refilter(&self) {
        self.0.borrow().filter.as_ref().map(|f| f.refilter());
    }
}
