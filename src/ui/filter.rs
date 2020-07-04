use crate::model::record::Record;
use crate::store::PSStore;
use gtk::prelude::*;

fn record_satisfies(
    store: &PSStore,
    filter_func: &dyn Fn(&Record) -> bool,
    iter: &gtk::TreeIter,
) -> bool {
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

fn tree_filter_func(
    store: &PSStore,
    iter: &gtk::TreeIter,
    filter_func: &dyn Fn(&Record) -> bool,
) -> bool {
    if record_satisfies(store, filter_func, iter) {
        return true;
    }
    for (_i, sup_record) in store.parents(iter) {
        if filter_func(&sup_record) {
            return true;
        }
    }
    false
}

pub fn create_model_filter(
    model: &PSStore,
    text: &str,
    look_at_secrets: bool,
) -> gtk::TreeModelFilter {
    let filter = gtk::TreeModelFilter::new(&model.as_model(), None);
    let text = text.to_owned();
    filter.set_visible_func(move |model, iter| {
        if let Some(store) = PSStore::from_tree_model(model) {
            let text = &text;
            tree_filter_func(&store, iter, &|record: &Record| {
                record.has_text(text, look_at_secrets)
            })
        } else {
            true
        }
    });
    filter
}
