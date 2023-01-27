use crate::gtk_prelude::*;
use std::cell::RefCell;

pub struct WeakMap<K: glib::ObjectType, V: glib::ObjectType> {
    mappings: RefCell<Vec<(glib::WeakRef<K>, glib::WeakRef<V>)>>,
}

impl<K: glib::ObjectType, V: glib::ObjectType> Default for WeakMap<K, V> {
    fn default() -> Self {
        Self {
            mappings: Default::default(),
        }
    }
}

impl<K: glib::ObjectType, V: glib::ObjectType> WeakMap<K, V> {
    pub fn add(&self, key: &K, value: &V) {
        self.clean_expired();
        let position = self
            .mappings
            .borrow()
            .iter()
            .position(|(k, _value)| k.upgrade().as_ref() == Some(key));
        match position {
            Some(pos) => {
                self.mappings.borrow_mut()[pos].1 = value.downgrade();
            }
            None => {
                self.mappings
                    .borrow_mut()
                    .push((key.downgrade(), value.downgrade()));
            }
        }
    }

    pub fn remove_key(&self, key: &K) {
        self.mappings
            .borrow_mut()
            .retain(|(k, v)| k.upgrade().as_ref() != Some(key) && v.upgrade().is_some());
    }

    pub fn remove_value(&self, value: &V) {
        self.mappings
            .borrow_mut()
            .retain(|(k, v)| k.upgrade().is_some() && v.upgrade().as_ref() != Some(value));
    }

    pub fn find(&self, key: &K) -> Option<V> {
        self.clean_expired();
        self.mappings
            .borrow()
            .iter()
            .find(|(k, _v)| k.upgrade().as_ref() == Some(key))
            .and_then(|(_k, v)| v.upgrade())
    }

    fn clean_expired(&self) {
        self.mappings
            .borrow_mut()
            .retain(|(key, value)| key.upgrade().is_some() && value.upgrade().is_some());
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[gtk::test]
    fn test_weak_map() {
        let key1 = gtk::StringObject::new("key1");
        let key2 = gtk::StringObject::new("key2");
        let widget1: gtk::Label = glib::Object::builder().build();
        let widget2: gtk::Label = glib::Object::builder().build();

        let map = WeakMap::<gtk::StringObject, gtk::Label>::default();

        map.add(&key1, &widget2);
        map.add(&key2, &widget2);
        map.add(&key1, &widget1); // replace

        assert_eq!(map.find(&key1).as_ref(), Some(&widget1));
        assert_eq!(map.find(&key2).as_ref(), Some(&widget2));

        drop(widget1);

        assert_eq!(map.find(&key1), None);
        assert_eq!(map.find(&key2).as_ref(), Some(&widget2));

        drop(widget2);
        assert_eq!(map.find(&key1), None);
        assert_eq!(map.find(&key2), None);
    }

    #[gtk::test]
    fn test_weak_map_remove() {
        let key1 = gtk::StringObject::new("key1");
        let key2 = gtk::StringObject::new("key2");
        let widget1: gtk::Label = glib::Object::builder().build();
        let widget2: gtk::Label = glib::Object::builder().build();

        let map = WeakMap::<gtk::StringObject, gtk::Label>::default();

        map.add(&key1, &widget1);
        map.add(&key2, &widget2);

        assert_eq!(map.find(&key1).as_ref(), Some(&widget1));
        assert_eq!(map.find(&key2).as_ref(), Some(&widget2));

        map.remove_key(&key1);

        assert_eq!(map.find(&key1), None);
        assert_eq!(map.find(&key2).as_ref(), Some(&widget2));

        map.remove_value(&widget2);

        assert_eq!(map.find(&key1), None);
        assert_eq!(map.find(&key2), None);
    }
}
