use crate::gtk_prelude::*;
use crate::utils::algorithm::all_equal_by_key;
use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    Text,
    MultiLine,
    Name,
    Password,
    Secret,
}

impl FieldType {
    pub fn is_secret(self) -> bool {
        matches!(self, FieldType::Password | FieldType::Secret)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Field {
    pub name: &'static str,
    pub title: &'static str,
    pub field_type: FieldType,
}

impl Field {
    const fn new(name: &'static str, title: &'static str, field_type: FieldType) -> Self {
        Field {
            name,
            title,
            field_type,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct RecordType {
    pub name: &'static str,
    pub title: &'static str,
    pub fields: Vec<Field>,
    pub is_group: bool,
    pub icon: &'static str,
    username_field: Option<&'static str>,
    password_field: Option<&'static str>,
}

#[derive(Debug, Clone, PartialEq, Eq, glib::Boxed)]
#[boxed_type(name = "PSRecord")]
pub struct Record {
    pub record_type: &'static RecordType,
    pub values: HashMap<String, String>,
}

pub const FIELD_NAME: Field = Field::new("name", "Name", FieldType::Text);
pub const FIELD_DESCRIPTION: Field = Field::new("description", "Description", FieldType::MultiLine);
pub const FIELD_HOSTNAME: Field = Field::new("hostname", "Hostname", FieldType::Text);
pub const FIELD_USERNAME: Field = Field::new("username", "Username", FieldType::Name);
pub const FIELD_PASSWORD: Field = Field::new("password", "Password", FieldType::Password);

pub static RECORD_TYPE_GROUP: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "group",
    title: "group",
    is_group: true,
    fields: vec![FIELD_NAME, FIELD_DESCRIPTION],
    icon: "folder",
    username_field: None,
    password_field: None,
});
pub static RECORD_TYPE_GENERIC: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "generic",
    title: "generic entry",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        FIELD_HOSTNAME,
        FIELD_USERNAME,
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-generic",
    username_field: Some("username"),
    password_field: Some("password"),
});
pub static RECORD_TYPE_CREDITCARD: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "creditcard",
    title: "credit card",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        Field::new("cardtype", "Card type", FieldType::Text),
        Field::new("cardnumber", "Card number", FieldType::Text),
        Field::new("expirydate", "Expiry date", FieldType::Secret),
        Field::new("ccv", "CCV number", FieldType::Secret),
        Field::new("pin", "PIN", FieldType::Secret),
        FIELD_DESCRIPTION,
    ],
    icon: "entry-creditcard",
    username_field: Some("cardnumber"),
    password_field: Some("ccv"),
});
pub static RECORD_TYPE_CRYPTOKEY: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "cryptokey",
    title: "crypto key",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        FIELD_HOSTNAME,
        Field::new("certificate", "Certificate", FieldType::Text),
        Field::new("keyfile", "Key file", FieldType::Text),
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-keyring",
    username_field: Some("hostname"),
    password_field: Some("password"),
});
pub static RECORD_TYPE_DATABASE: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "database",
    title: "database",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        FIELD_HOSTNAME,
        Field::new("database", "Database", FieldType::Text),
        FIELD_USERNAME,
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-database",
    username_field: Some("username"),
    password_field: Some("password"),
});
pub static RECORD_TYPE_DOOR: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "door",
    title: "door",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        Field::new("location", "Location", FieldType::Text),
        Field::new("code", "Code", FieldType::Secret),
        FIELD_DESCRIPTION,
    ],
    icon: "entry-door",
    username_field: Some("location"),
    password_field: Some("code"),
});
pub static RECORD_TYPE_EMAIL: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "email",
    title: "e-mail",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        Field::new("email", "E-mail", FieldType::Text),
        FIELD_HOSTNAME,
        FIELD_USERNAME,
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-email",
    username_field: Some("username"),
    password_field: Some("password"),
});
pub static RECORD_TYPE_FTP: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "ftp",
    title: "FTP",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        FIELD_HOSTNAME,
        Field::new("port", "Port", FieldType::Text),
        FIELD_USERNAME,
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-ftp",
    username_field: Some("username"),
    password_field: Some("password"),
});
pub static RECORD_TYPE_PHONE: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "phone",
    title: "phone",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        Field::new("phonenumber", "Number", FieldType::Text),
        Field::new("pin", "PIN", FieldType::Secret),
        FIELD_DESCRIPTION,
    ],
    icon: "entry-phone",
    username_field: Some("phonenumber"),
    password_field: Some("pin"),
});
pub static RECORD_TYPE_SHELL: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "shell",
    title: "shell",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        FIELD_HOSTNAME,
        Field::new("domain", "Domain", FieldType::Text),
        FIELD_USERNAME,
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-shell",
    username_field: Some("username"),
    password_field: Some("password"),
});
pub static RECORD_TYPE_WEBSITE: Lazy<RecordType> = Lazy::new(|| RecordType {
    name: "website",
    title: "website",
    is_group: false,
    fields: vec![
        FIELD_NAME,
        Field::new("url", "URL", FieldType::Text),
        FIELD_USERNAME,
        FIELD_PASSWORD,
        FIELD_DESCRIPTION,
    ],
    icon: "entry-website",
    username_field: Some("username"),
    password_field: Some("password"),
});
pub static RECORD_TYPES: Lazy<Vec<&'static RecordType>> = Lazy::new(|| {
    vec![
        &RECORD_TYPE_GROUP,
        &RECORD_TYPE_GENERIC,
        &RECORD_TYPE_CREDITCARD,
        &RECORD_TYPE_CRYPTOKEY,
        &RECORD_TYPE_DATABASE,
        &RECORD_TYPE_DOOR,
        &RECORD_TYPE_EMAIL,
        &RECORD_TYPE_FTP,
        &RECORD_TYPE_PHONE,
        &RECORD_TYPE_SHELL,
        &RECORD_TYPE_WEBSITE,
    ]
});

pub const RECORD_TYPE_FIELD: &str = "__record_type";

impl RecordType {
    pub fn find(name: &str) -> Option<&'static RecordType> {
        RECORD_TYPES.iter().find(|r| r.name == name).copied()
    }

    pub fn new_record(&'static self) -> Record {
        let mut values = HashMap::new();
        values.insert(RECORD_TYPE_FIELD.to_owned(), self.name.to_owned());
        for field in &self.fields {
            values.insert(field.name.to_owned(), String::new());
        }
        Record {
            record_type: self,
            values,
        }
    }
}

impl Record {
    pub fn name(&self) -> String {
        self.values.get("name").cloned().unwrap_or_default()
    }

    pub fn description(&self) -> String {
        self.values.get("description").cloned().unwrap_or_default()
    }

    pub fn get_field(&self, field: &Field) -> &str {
        self.values
            .get(field.name)
            .map_or("", |value| value.as_str())
    }

    pub fn set_field(&mut self, field: &Field, value: &str) {
        self.values.insert(field.name.to_owned(), value.to_owned());
    }

    pub fn username(&self) -> Option<&str> {
        self.record_type
            .username_field
            .and_then(|field| self.values.get(field))
            .map(|value| value.as_str())
    }

    pub fn password(&self) -> Option<&str> {
        self.record_type
            .password_field
            .and_then(|field| self.values.get(field))
            .map(|value| value.as_str())
    }

    pub fn url(&self) -> Option<&str> {
        self.record_type
            .fields
            .iter()
            .find(|f| f.name == "url")
            .map(|field| self.get_field(field).trim())
            .filter(|value| !value.is_empty())
    }

    pub fn has_text(&self, text: &str, look_at_secrets: bool) -> bool {
        let needle = text.to_lowercase();
        for field in &self.record_type.fields {
            let is_field_searchable = look_at_secrets || !field.field_type.is_secret();
            if is_field_searchable {
                let field_value = self.get_field(field);
                if field_value.to_lowercase().contains(&needle) {
                    return true;
                }
            }
        }
        false
    }

    pub fn rename<F: Fn(&str) -> String>(&mut self, update_name: F) {
        let old_name = self.get_field(&FIELD_NAME);
        let new_name = update_name(old_name);
        self.set_field(&FIELD_NAME, &new_name);
    }

    pub fn join_entries(records: &[Record]) -> Record {
        let record_type = if let Some(record) = all_equal_by_key(records, |r| r.record_type) {
            record.record_type
        } else {
            &RECORD_TYPE_GENERIC
        };
        let name = if let Some(record) = all_equal_by_key(records, |r| r.name()) {
            record.name()
        } else {
            records
                .iter()
                .map(|p| p.name())
                .collect::<Vec<_>>()
                .join(" and ")
        };

        let mut result = record_type.new_record();
        for record in records.iter() {
            result.join(record);
        }
        result.set_field(&FIELD_NAME, &name);
        result
    }

    pub fn join(&mut self, record: &Record) {
        let mut unmapped = Vec::new();
        for src_field in &record.record_type.fields {
            if src_field.name != FIELD_NAME.name && src_field.name != FIELD_DESCRIPTION.name {
                let value = record.get_field(src_field);
                if !value.is_empty() {
                    if let Some(dst_field) = self
                        .record_type
                        .fields
                        .iter()
                        .find(|f| f.name == src_field.name)
                    {
                        let dst_value = self.get_field(dst_field);
                        if dst_value.is_empty() {
                            self.set_field(dst_field, value);
                        } else if dst_value != value {
                            unmapped.push(src_field);
                        }
                    } else {
                        unmapped.push(src_field);
                    }
                }
            }
        }

        {
            let desc = record.get_field(&FIELD_DESCRIPTION);
            if !desc.is_empty() {
                unmapped.insert(0, &FIELD_DESCRIPTION);
            }
        }

        if !unmapped.is_empty() {
            let mut desc = String::new();
            desc.push_str(self.get_field(&FIELD_DESCRIPTION));

            if !desc.is_empty() {
                desc.push_str("\n\n");
            }
            desc.push_str(record.get_field(&FIELD_NAME));
            desc.push('\n');
            desc.push_str("----------");
            desc.push('\n');
            for field in unmapped {
                desc.push_str(field.title);
                desc.push_str(": ");
                desc.push_str(record.get_field(field));
                desc.push('\n');
            }

            self.set_field(&FIELD_DESCRIPTION, &desc);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fields() {
        let group = RecordType::find("group").unwrap().new_record();
        let generic = RECORD_TYPE_GENERIC.new_record();

        assert_eq!(group.record_type.name, "group");
        assert_eq!(group.values.keys().len(), 2 + 1);

        assert_eq!(generic.record_type.name, "generic");
        assert_eq!(generic.values.keys().len(), 5 + 1);
    }
}
