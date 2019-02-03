use std::ops::Drop;
use std::cmp::{PartialEq, Eq};
use std::clone::Clone;
use std::convert::Into;
use std::fmt;
use std::iter::Iterator;
use glib::Value;
use glib::types::Type;
use glib::translate::{FromGlib, FromGlibPtrNone, Uninitialized, ToGlib, ToGlibPtr, ToGlibPtrMut};
use glib_sys::{
    GHashTable,
    GHashTableIter,
    g_hash_table_get_type,
    g_hash_table_new_full,
    g_hash_table_ref,
    g_hash_table_unref,
    g_hash_table_insert,
    g_hash_table_lookup,
    g_hash_table_iter_init,
    g_hash_table_iter_next,
    g_str_hash,
    g_str_equal,
    g_free,
    gpointer,
    gconstpointer,
};
use gobject_sys::{
    g_value_init,
    g_value_set_boxed_take_ownership,
    g_value_get_boxed,
};
use libc::{c_char, c_void};

pub struct HashTable(*mut GHashTable);

pub struct HashTableIter<'h>(GHashTableIter, &'h HashTable);

impl<'h> Iterator for HashTableIter<'h> {
    type Item = (String, String);

    fn next(&mut self) -> Option<Self::Item> {
        let mut key: *mut c_void = std::ptr::null_mut::<c_void>();
        let mut value: *mut c_void = std::ptr::null_mut::<c_void>();
        if unsafe { g_hash_table_iter_next(&mut self.0 as *mut GHashTableIter, &mut key, &mut value) } != 0 {
            Some((
                unsafe { String::from_glib_none(key as *const c_char) },
                unsafe { String::from_glib_none(value as *const c_char) }
            ))
        } else {
            None
        }
    }
}

impl HashTable {
    pub fn glib_type() -> Type {
        Type::from_glib(unsafe { g_hash_table_get_type() })
    }

    pub fn new() -> Self {
        let ptr = unsafe { g_hash_table_new_full(Some(g_str_hash), Some(g_str_equal), Some(g_free), Some(g_free)) };
        HashTable(ptr)
    }

    pub fn insert(&mut self, key: &str, val: &str) {
        let k: *mut c_char = key.to_glib_full();
        let v: *mut c_char = val.to_glib_full();
        unsafe { g_hash_table_insert(self.0, k as gpointer, v as gpointer); }
    }

    pub fn get(&self, key: &str) -> Option<String> {
        let k: *mut c_char = key.to_glib_full();
        let v = unsafe { g_hash_table_lookup(self.0, k as gconstpointer) } as *const c_char;
        if !v.is_null() {
            Some(unsafe { String::from_glib_none(v) })
        } else {
            None
        }
    }

    pub fn iter(&self) -> HashTableIter<'_> {
        unsafe {
            let mut iter = GHashTableIter {
                dummy1: std::ptr::null_mut::<c_void>(),
                dummy2: std::ptr::null_mut::<c_void>(),
                dummy3: std::ptr::null_mut::<c_void>(),
                dummy4: 0,
                dummy5: 0,
                dummy6: std::ptr::null_mut::<c_void>(),
            };
            g_hash_table_iter_init(&mut iter as *mut GHashTableIter, self.0);

            HashTableIter(iter, self)
        }
    }

    pub fn keys(&self) -> Vec<String> {
        self.iter().map(|p| p.0).collect()
    }

    pub fn contains(&self, other: &HashTable) -> bool {
        for (key, value) in other.iter() {
            if self.get(&key) != Some(value) {
                return false;
            }
        }
        true
    }

    pub fn try_from_value(value: &Value) -> Option<HashTable> {
        if value.type_().to_glib() != unsafe { g_hash_table_get_type() } {
            return None;
        }
        let gvalue = value.to_glib_none().0;
        if gvalue.is_null() {
            return None;
        }
        let ptr = unsafe { g_value_get_boxed(gvalue) } as *mut GHashTable;
        if ptr.is_null() {
            return None;
        }
        let ptr_ref = unsafe { g_hash_table_ref(ptr) };
        Some(HashTable(ptr_ref))
    }
}

impl Drop for HashTable {
    fn drop(&mut self) {
        unsafe { g_hash_table_unref(self.0); }
    }
}

impl Clone for HashTable {
    fn clone(&self) -> Self {
        let mut clone = Self::new();
        for (key, value) in self.iter() {
            clone.insert(&key, &value);
        }
        clone
    }
}

impl PartialEq<HashTable> for HashTable {
    fn eq(&self, other: &HashTable) -> bool {
        self.contains(other) && other.contains(self)
    }
}

impl Eq for HashTable {}

impl fmt::Debug for HashTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let mut s = f.debug_struct("HashTable");
        for (key, value) in self.iter() {
            s.field(&key, &value);
        }
        s.finish()
    }
}

impl Into<Value> for HashTable {
    fn into(self) -> Value {
        unsafe {
            let mut ret = Value::uninitialized();
            g_value_init(ret.to_glib_none_mut().0, g_hash_table_get_type());
            let ptr = g_hash_table_ref(self.0);
            g_value_set_boxed_take_ownership(ret.to_glib_none_mut().0, ptr as gconstpointer);
            ret
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn to_value_and_back() {
        let mut generic = HashTable::new();
        assert!(generic.keys().is_empty());
        generic.insert("username", "andy");
        assert_eq!(generic.keys(), vec!["username".to_string()]);
        generic.insert("url", "http://example.com");

        let mut keys = generic.keys();
        keys.sort();
        assert_eq!(keys, vec!["url".to_string(), "username".to_string()]);

        let value: Value = generic.into();

        let generic2 = HashTable::try_from_value(&value).unwrap();

        assert_eq!(generic2.get("username"), Some("andy".to_string()));
        assert_eq!(generic2.get("password"), None);

        let mut keys = generic2.keys();
        keys.sort();
        assert_eq!(keys, vec!["url".to_string(), "username".to_string()]);
    }

    #[test]
    fn test_hashtable_debug() {
        let mut generic = HashTable::new();
        generic.insert("username", "andy");
        generic.insert("url", "http://example.com");

        assert_eq!(format!("{:?}", generic), r##"HashTable { url: "http://example.com", username: "andy" }"##.to_string());
    }
}
