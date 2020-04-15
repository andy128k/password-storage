use glib::clone;
use gtk::prelude::*;
use gtk::{TreeModel, TreeModelFilter, TreeModelFilterExt, TreeIter};
use crate::model::record::Record;
use crate::store::PSStore;
use crate::ptr::SharedPtr;

pub struct PSTreeFilterPrivate {
    model: Option<PSStore>,
    filter: Option<TreeModelFilter>,
    filter_func: Option<Box<dyn Fn(&Record) -> bool>>,
}

pub type PSTreeFilter = SharedPtr<PSTreeFilterPrivate>;

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
        Self::from_private(private)
    }

    pub fn as_model(&self) -> Option<TreeModel> {
        self.borrow().filter.clone().map(|m| m.upcast())
    }

    pub fn set_model(&self, model: Option<&PSStore>) {
        match model {
            Some(model) => {
                let filter = TreeModelFilter::new(&model.as_model(), None);
                filter.set_visible_func(clone!(@weak self as this => @default-return true, move |_model, iter| {
                    let private = this.borrow();
                    tree_filter_func(&private, iter)
                }));
                self.borrow_mut().model = Some(model.clone());
                self.borrow_mut().filter = Some(filter);
            },
            None => {
                self.borrow_mut().model = None;
                self.borrow_mut().filter = None;
            }
        };
    }

    pub fn set_filter_func(&self, filter_func: Option<Box<dyn Fn(&Record) -> bool>>) {
        self.borrow_mut().filter_func = filter_func;
    }

    pub fn refilter(&self) {
        if let Some(ref model_filter) = self.borrow().filter {
            model_filter.refilter();
        }
    }
}
