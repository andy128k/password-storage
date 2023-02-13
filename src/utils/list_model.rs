use gtk::{gio, gio::prelude::*, glib};
use std::ops::{Bound, RangeBounds};

pub trait ListModelImmutableExt {
    fn first(&self) -> Option<glib::Object>;
    fn last(&self) -> Option<glib::Object>;
    fn sliced(&self, range: impl RangeBounds<u32>) -> gio::ListStore;

    fn appended<T: glib::IsA<glib::Object>>(&self, new_item: &T) -> gio::ListStore {
        let result = self.sliced(..);
        result.append(new_item);
        result
    }
}

impl<T: glib::IsA<gio::ListModel>> ListModelImmutableExt for T {
    fn first(&self) -> Option<glib::Object> {
        let size = self.n_items();
        if size > 0 {
            self.item(0)
        } else {
            None
        }
    }

    fn last(&self) -> Option<glib::Object> {
        let size = self.n_items();
        if size > 0 {
            self.item(size - 1)
        } else {
            None
        }
    }

    fn sliced(&self, range: impl RangeBounds<u32>) -> gio::ListStore {
        let start_index = match range.start_bound() {
            Bound::Unbounded => 0,
            Bound::Included(index) => *index,
            Bound::Excluded(index) => index + 1,
        };
        let end_index = match range.end_bound() {
            Bound::Unbounded => self.n_items(),
            Bound::Included(index) => index + 1,
            Bound::Excluded(index) => *index,
        };
        let result = gio::ListStore::new(self.item_type());
        for index in start_index..end_index {
            if let Some(item) = self.item(index) {
                result.append(&item);
            }
        }
        result
    }
}
