use std::convert::Into;
use glib::Value;
use glib::types::Type;
use glib::translate::{FromGlib, FromGlibPtrNone, Uninitialized, ToGlib, ToGlibPtr, ToGlibPtrMut};
use glib_sys::{GType, GHashTable, g_hash_table_get_type, g_hash_table_new_full, g_hash_table_insert, g_hash_table_lookup, g_str_hash, g_str_equal, g_free};
use glib_sys::{gpointer, gconstpointer};
use gobject_sys::{g_value_init, g_value_set_boxed_take_ownership, g_value_get_boxed};
use libc::{c_char};
use model::record::{Record, RecordType};

const RECORD_TYPE_FIELD: &str = "__record_type";

unsafe fn hashtable_insert(table: *mut GHashTable, key: &str, val: &str) {
    let k: *mut c_char = key.to_glib_full();
    let v: *mut c_char = val.to_glib_full();
    g_hash_table_insert(table, k as gpointer, v as gpointer);
}

unsafe fn hashtable_lookup(table: *mut GHashTable, key: &str) -> String {
    let k: *mut c_char = key.to_glib_full();
    let v = g_hash_table_lookup(table, k as gconstpointer) as *const c_char;
    if !v.is_null() {
        String::from_glib_none(v)
    } else {
        String::new()
    }
}

unsafe fn hashtable_from_value(value: &Value) -> Option<*mut GHashTable> {
    if value.type_().to_glib() != g_hash_table_get_type() {
        return None;
    }
    let gvalue = value.to_glib_none().0;
    if gvalue.is_null() {
        return None;
    }
    let ptr = g_value_get_boxed(gvalue) as *mut GHashTable;
    if ptr.is_null() {
        return None;
    }
    Some(ptr)
}

unsafe fn hashtable_to_value(table: *mut GHashTable) -> Value {
    let mut ret = Value::uninitialized();
    g_value_init(ret.to_glib_none_mut().0, g_hash_table_get_type());
    g_value_set_boxed_take_ownership(ret.to_glib_none_mut().0, table as gconstpointer);
    ret
}

pub fn record_glib_type() -> Type {
    let gtype: GType;
    unsafe {
        gtype = g_hash_table_get_type();
    }
    Type::from_glib(gtype)
}

impl Record {
    pub fn try_from_value(value: &Value) -> Option<Self> {
        unsafe {
            if let Some(table) = hashtable_from_value(value) {
                let record_type_name = hashtable_lookup(table, RECORD_TYPE_FIELD);
                if let Some(record_type) = RecordType::find(&record_type_name) {
                    let mut record = record_type.new_record();
                    for field in &record_type.fields {
                        record.values.insert(field.name.to_string(), hashtable_lookup(table, field.name));
                    }
                    return Some(record)
                }
            }
            None
        }
    }
}

impl<'any> Into<Value> for &'any Record {
    fn into(self) -> Value {
        unsafe {
            let ptr = g_hash_table_new_full(Some(g_str_hash), Some(g_str_equal), Some(g_free), Some(g_free));
            for (k, v) in &self.values {
                hashtable_insert(ptr, k, v);
            }
            hashtable_insert(ptr, RECORD_TYPE_FIELD, self.record_type.name);
            hashtable_to_value(ptr)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use model::record::*;

    #[test]
    fn to_glib_and_back() {
        let mut generic = RECORD_TYPE_GENERIC.new_record();
        generic.values.insert("username".to_string(), "andy".to_string());
        let value: Value = (&generic).into();
        let generic2 = Record::try_from_value(&value).unwrap();
        assert_eq!(generic2.values.get(&"username".to_string()), Some(&"andy".to_string()));
        assert_eq!(generic2.values.get(&"password".to_string()), Some(&"".to_string()));
    }

    #[test]
    fn to_glib_and_back_website() {
        let mut generic = RECORD_TYPE_WEBSITE.new_record();
        generic.values.insert("username".to_string(), "andy".to_string());
        let value: Value = (&generic).into();
        let generic2 = Record::try_from_value(&value).unwrap();
        assert_eq!(generic2.values.get(&"username".to_string()), Some(&"andy".to_string()));
        assert_eq!(generic2.values.get(&"password".to_string()), Some(&"".to_string()));
    }
}
