use gtk::{glib, glib::prelude::*};
use std::cell::RefCell;

pub struct WeakMap<K: Copy, V: glib::ObjectType> {
    mappings: RefCell<Vec<(K, glib::WeakRef<V>)>>,
}

impl<K: Copy, V: glib::ObjectType> Default for WeakMap<K, V> {
    fn default() -> Self {
        Self {
            mappings: Default::default(),
        }
    }
}

impl<K: Copy + PartialEq, V: glib::ObjectType> WeakMap<K, V> {
    pub fn add(&self, key: K, value: &V) {
        self.clean_expired();
        let position = self
            .mappings
            .borrow()
            .iter()
            .position(|(k, _value)| *k == key);
        match position {
            Some(pos) => {
                self.mappings.borrow_mut()[pos].1 = value.downgrade();
            }
            None => {
                self.mappings.borrow_mut().push((key, value.downgrade()));
            }
        }
    }

    pub fn remove_key(&self, key: K) {
        self.mappings
            .borrow_mut()
            .retain(|(k, v)| *k != key && v.upgrade().is_some());
    }

    pub fn remove_value(&self, value: &V) {
        self.mappings.borrow_mut().retain(|(_k, v)| {
            let v = v.upgrade();
            v.is_some() && v.as_ref() != Some(value)
        });
    }

    pub fn find(&self, key: K) -> Option<V> {
        self.clean_expired();
        self.mappings
            .borrow()
            .iter()
            .find(|(k, _v)| *k == key)
            .and_then(|(_k, v)| v.upgrade())
    }

    fn clean_expired(&self) {
        self.mappings
            .borrow_mut()
            .retain(|(_key, value)| value.upgrade().is_some());
    }
}
