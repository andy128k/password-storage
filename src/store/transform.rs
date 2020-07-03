use crate::model::record::{Record, RecordType, RECORD_TYPE_FIELD};
use crate::utils::hash_table::*;
use glib::Value;
use std::convert::Into;

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

impl<'any> Into<Value> for Record {
    fn into(self) -> Value {
        self.values.into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::model::record::*;

    #[test]
    fn to_glib_and_back() {
        let mut generic = RECORD_TYPE_GENERIC.new_record();
        generic.values.insert("username", "andy");
        let value: Value = generic.into();
        let generic2 = Record::try_from_value(&value).unwrap();
        assert_eq!(generic2.values.get("username"), Some("andy".to_string()));
        assert_eq!(generic2.values.get("password"), Some("".to_string()));
    }

    #[test]
    fn to_glib_and_back_website() {
        let mut generic = RECORD_TYPE_WEBSITE.new_record();
        generic.values.insert("username", "andy");
        let value: Value = generic.into();
        let generic2 = Record::try_from_value(&value).unwrap();
        assert_eq!(generic2.values.get("username"), Some("andy".to_string()));
        assert_eq!(generic2.values.get("password"), Some("".to_string()));
    }
}
