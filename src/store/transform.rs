use std::convert::Into;
use glib::Value;
use glib::types::Type;
use glib::translate::{FromGlib};
use glib_sys::{g_hash_table_get_type};
use model::record::{Record, RecordType, RECORD_TYPE_FIELD};
use utils::hash_table::*;

pub fn record_glib_type() -> Type {
    Type::from_glib(unsafe { g_hash_table_get_type() })
}

impl Record {
    pub fn try_from_value(value: &Value) -> Option<Self> {
        let table = HashTable::try_from_value(value)?;
        let record_type_name = table.get(RECORD_TYPE_FIELD)?;
        let record_type = RecordType::find(&record_type_name)?;
        let mut record = record_type.new_record();
        record.values = table;
        Some(record)
    }
}

impl<'any> Into<Value> for &'any Record {
    fn into(self) -> Value {
        self.values.clone().into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use model::record::*;

    #[test]
    fn to_glib_and_back() {
        let mut generic = RECORD_TYPE_GENERIC.new_record();
        generic.values.insert("username", "andy");
        let value: Value = (&generic).into();
        let generic2 = Record::try_from_value(&value).unwrap();
        assert_eq!(generic2.values.get("username"), Some("andy".to_string()));
        assert_eq!(generic2.values.get("password"), Some("".to_string()));
    }

    #[test]
    fn to_glib_and_back_website() {
        let mut generic = RECORD_TYPE_WEBSITE.new_record();
        generic.values.insert("username", "andy");
        let value: Value = (&generic).into();
        let generic2 = Record::try_from_value(&value).unwrap();
        assert_eq!(generic2.values.get("username"), Some("andy".to_string()));
        assert_eq!(generic2.values.get("password"), Some("".to_string()));
    }
}
