use gtk::{gio, gio::prelude::*, glib};
use std::iter::once;
use std::marker::PhantomData;

#[derive(glib::Downgrade, Debug, Clone)]
pub struct TypedListStore<T>(gio::ListStore, PhantomData<T>);

impl<T> TypedListStore<T>
where
    T: glib::IsA<glib::Object>,
{
    pub fn from_untyped(list: gio::ListStore) -> Self {
        Self(list, PhantomData)
    }

    pub fn new() -> Self {
        Self(gio::ListStore::new::<T>(), PhantomData)
    }

    pub fn untyped(&self) -> &gio::ListStore {
        &self.0
    }

    pub fn into_untyped(self) -> gio::ListStore {
        self.0
    }

    pub fn len(&self) -> u32 {
        self.0.n_items()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, index: u32) -> Option<T> {
        self.0.item(index).and_downcast()
    }

    pub fn last(&self) -> Option<T> {
        self.get(self.len().saturating_sub(1))
    }

    pub fn set(&self, index: u32, value: T) {
        self.0.splice(index, 1, &[value.upcast()]);
    }

    pub fn truncate(&self, len: u32) {
        if len < self.len() {
            const NOTHING: &[glib::Object] = &[];
            self.0.splice(len, self.len() - len, NOTHING);
        }
    }

    pub fn updated(self, index: u32, value: T) -> Self {
        if self.get(index).as_ref() == Some(&value) {
            self
        } else {
            self.iter()
                .enumerate()
                .map(|p| {
                    if p.0 as u32 == index {
                        value.clone()
                    } else {
                        p.1
                    }
                })
                .collect()
        }
    }

    pub fn update_by(&self, updater: impl Fn(&T) -> Option<T>) {
        for index in 0..self.len() {
            if let Some(value) = self.get(index) {
                if let Some(new_value) = updater(&value) {
                    if new_value != value {
                        self.set(index, new_value);
                    }
                }
            }
        }
    }

    pub fn updated_by(&self, updater: impl Fn(&T) -> Option<T>) -> Self {
        let mut store = self.clone();
        for index in 0..self.len() {
            if let Some(new_value) = self.get(index).and_then(|value| updater(&value)) {
                store = store.updated(index, new_value);
            }
        }
        store
    }

    pub fn remove(&self, index: u32) -> Option<T> {
        let item = self.get(index)?;
        const NOTHING: &[glib::Object] = &[];
        self.0.splice(index, 1, NOTHING);
        Some(item)
    }

    pub fn removed(&self, index: usize) -> Self {
        self.iter()
            .enumerate()
            .filter(|p| p.0 != index)
            .map(|p| p.1)
            .collect()
    }

    pub fn pop_back(&self) -> Option<T> {
        self.remove(self.len().saturating_sub(1))
    }

    pub fn remove_all(&self) {
        self.0.remove_all();
    }

    pub fn append(&self, obj: &T) {
        self.0.append(obj);
    }

    pub fn appended(&self, value: T) -> Self {
        self.iter().chain(once(value)).collect()
    }

    pub fn insert(&self, index: impl Into<Option<usize>>, obj: &T) {
        match index.into() {
            Some(index) => self.0.insert(index as u32, obj),
            None => self.append(obj),
        }
    }

    pub fn inserted(&self, index: usize, value: T) -> Self {
        let mut vec: Vec<T> = self.iter().collect();
        vec.insert(index, value);
        vec.into_iter().collect()
    }

    pub fn with(self, obj: &T) -> Self {
        self.append(obj);
        self
    }

    pub fn iter(&self) -> ListStoreIterator<T> {
        ListStoreIterator {
            list_store: self.clone(),
            index: 0,
        }
    }

    pub fn clone_list(&self) -> Self {
        self.iter().collect()
    }

    pub fn take_if(&self, predicate: impl Fn(&T) -> bool) -> Option<T> {
        let index = self.iter().position(|item| predicate(&item))? as u32;
        let found = self.get(index);
        self.remove(index);
        found
    }

    pub fn find(&self, item: &T) -> Option<u32> {
        self.0.find(item)
    }
}

impl<T> std::default::Default for TypedListStore<T>
where
    T: glib::IsA<glib::Object>,
{
    fn default() -> Self {
        Self::new()
    }
}

pub struct ListStoreIterator<T> {
    list_store: TypedListStore<T>,
    index: u32,
}

impl<T> std::iter::Iterator for ListStoreIterator<T>
where
    T: glib::IsA<glib::Object>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.list_store.get(self.index);
        if value.is_some() {
            self.index += 1;
        }
        value
    }
}

impl<T> std::iter::IntoIterator for &TypedListStore<T>
where
    T: glib::IsA<glib::Object>,
{
    type Item = T;
    type IntoIter = ListStoreIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> std::iter::FromIterator<T> for TypedListStore<T>
where
    T: glib::IsA<glib::Object>,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let list = TypedListStore::new();
        for item in iter {
            list.append(&item);
        }
        list
    }
}

impl<T> std::cmp::PartialEq for TypedListStore<T>
where
    T: glib::IsA<glib::Object> + PartialEq,
{
    fn eq(&self, other: &TypedListStore<T>) -> bool {
        let len = self.len();
        if len != other.len() {
            return false;
        }
        for i in 0..len {
            if self.get(i) != other.get(i) {
                return false;
            }
        }
        true
    }
}
