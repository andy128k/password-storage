use lazy_static::lazy_static;
use crate::utils::hash_table::HashTable;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    Text,
    MultiLine,
    Name,
    Password,
    Secret
}

impl FieldType {
    pub fn is_secret(self) -> bool {
        match self {
            FieldType::Password | FieldType::Secret => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Field {
    pub name: &'static str,
    pub title: &'static str,
    pub field_type: FieldType,
}

impl Field {
    fn new(name: &'static str, title: &'static str, field_type: FieldType) -> Self {
        Field { name, title, field_type }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub record_type: &'static RecordType,
    pub values: HashTable,
}

lazy_static!{
    pub static ref FIELD_NAME: Field = Field::new("name", "Name", FieldType::Text);
    pub static ref FIELD_DESCRIPTION: Field = Field::new("description", "Description", FieldType::MultiLine);
    pub static ref FIELD_HOSTNAME: Field = Field::new("hostname", "Hostname", FieldType::Text);
    pub static ref FIELD_USERNAME: Field = Field::new("username", "Username", FieldType::Name);
    pub static ref FIELD_PASSWORD: Field = Field::new("password", "Password", FieldType::Password);

    pub static ref RECORD_TYPE_GROUP: RecordType = RecordType {
        name: "group",
        title: "group",
        is_group: true,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
        ],
        icon: "folder",
        username_field: None,
        password_field: None,
    };

    pub static ref RECORD_TYPE_GENERIC: RecordType = RecordType {
        name: "generic",
        title: "generic entry",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            *FIELD_HOSTNAME,
            *FIELD_USERNAME,
            *FIELD_PASSWORD,
        ],
        icon: "entry-generic",
        username_field: Some("username"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPE_CREDITCARD: RecordType = RecordType {
        name: "creditcard",
        title: "credit card",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            Field::new("cardtype", "Card type", FieldType::Text),
            Field::new("cardnumber", "Card number", FieldType::Text),
            Field::new("expirydate", "Expiry date", FieldType::Secret),
            Field::new("ccv", "CCV number", FieldType::Secret),
            Field::new("pin", "PIN", FieldType::Secret),
        ],
        icon: "entry-creditcard",
        username_field: Some("cardnumber"),
        password_field: Some("ccv"),
    };

    pub static ref RECORD_TYPE_CRYPTOKEY: RecordType = RecordType {
        name: "cryptokey",
        title: "crypto key",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            *FIELD_HOSTNAME,
            Field::new("certificate", "Certificate", FieldType::Text),
            Field::new("keyfile", "Key file", FieldType::Text),
            *FIELD_PASSWORD,
        ],
        icon: "entry-keyring",
        username_field: Some("hostname"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPE_DATABASE: RecordType = RecordType {
        name: "database",
        title: "database",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            *FIELD_HOSTNAME,
            *FIELD_USERNAME,
            *FIELD_PASSWORD,
            Field::new("database", "Database", FieldType::Text),
        ],
        icon: "entry-database",
        username_field: Some("username"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPE_DOOR: RecordType = RecordType {
        name: "door",
        title: "door",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            Field::new("location", "Location", FieldType::Text),
            Field::new("code", "Code", FieldType::Secret),
        ],
        icon: "entry-door",
        username_field: Some("location"),
        password_field: Some("code"),
    };

    pub static ref RECORD_TYPE_EMAIL: RecordType = RecordType {
        name: "email",
        title: "e-mail",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            Field::new("email", "E-mail", FieldType::Text),
            *FIELD_HOSTNAME,
            *FIELD_USERNAME,
            *FIELD_PASSWORD,
        ],
        icon: "entry-email",
        username_field: Some("username"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPE_FTP: RecordType = RecordType {
        name: "ftp",
        title: "FTP",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            *FIELD_HOSTNAME,
            Field::new("port", "Port", FieldType::Text),
            *FIELD_USERNAME,
            *FIELD_PASSWORD,
        ],
        icon: "entry-ftp",
        username_field: Some("username"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPE_PHONE: RecordType = RecordType {
        name: "phone",
        title: "phone",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            Field::new("phonenumber", "Number", FieldType::Text),
            Field::new("pin", "PIN", FieldType::Secret),
        ],
        icon: "entry-phone",
        username_field: Some("phonenumber"),
        password_field: Some("pin"),
    };

    pub static ref RECORD_TYPE_SHELL: RecordType = RecordType {
        name: "shell",
        title: "shell",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            *FIELD_HOSTNAME,
            Field::new("domain", "Domain", FieldType::Text),
            *FIELD_USERNAME,
            *FIELD_PASSWORD,
        ],
        icon: "entry-shell",
        username_field: Some("username"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPE_WEBSITE: RecordType = RecordType {
        name: "website",
        title: "website",
        is_group: false,
        fields: vec![
            *FIELD_NAME,
            *FIELD_DESCRIPTION,
            Field::new("url", "URL", FieldType::Text),
            *FIELD_USERNAME,
            *FIELD_PASSWORD,
        ],
        icon: "entry-website",
        username_field: Some("username"),
        password_field: Some("password"),
    };

    pub static ref RECORD_TYPES: Vec<&'static RecordType> = vec![
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
    ];
}

pub const RECORD_TYPE_FIELD: &str = "__record_type";

impl RecordType {
    pub fn find(name: &str) -> Option<&'static RecordType> {
        for record_type in RECORD_TYPES.iter() {
            if record_type.name == name {
                return Some(record_type)
            }
        }
        None
    }

    pub fn new_record(&'static self) -> Record {
        let mut values = HashTable::new();
        values.insert(RECORD_TYPE_FIELD, &self.name);
        for field in &self.fields {
            values.insert(field.name, "");
        }
        Record { record_type: self, values }
    }

    pub fn ref_eq(&'static self, other: &'static Self) -> bool {
        (self as * const _) == (other as * const _)
    }
}

fn common_type(types: &[&'static RecordType]) -> &'static RecordType {
    if let Some((first, rest)) = types.split_first() {
        if rest.iter().all(|item| RecordType::ref_eq(item, first)) {
            return first;
        }
    }
    &RECORD_TYPE_GENERIC
}

fn common_name(names: &[String]) -> String {
    if let Some((first, rest)) = names.split_first() {
        if rest.iter().all(|name| name == first) {
            return first.to_string();
        }
    }
    names.join(" and ")
}

impl Record {
    pub fn name(&self) -> String {
        self.values.get("name").unwrap_or_default()
    }

    pub fn get_field(&self, field: &Field) -> String {
        match self.values.get(field.name) {
            Some(name) => name.to_string(),
            None => String::new()
        }
    }

    pub fn set_field(&mut self, field: &Field, value: &str) {
        self.values.insert(field.name, value);
    }

    pub fn username(&self) -> Option<String> {
        self.record_type.username_field
            .and_then(|field| self.values.get(field))
    }

    pub fn password(&self) -> Option<String> {
        self.record_type.password_field
            .and_then(|field| self.values.get(field))
    }

    pub fn has_text(&self, text: &str, look_at_secrets: bool) -> bool {
        let needle = text.to_lowercase();
        for field in &self.record_type.fields {
            if look_at_secrets || !field.field_type.is_secret() {
                if self.get_field(field).to_lowercase().contains(&needle) {
                    return true;
                }
            }
        }
        false
    }

    pub fn rename<F: Fn(&str) -> String>(&mut self, update_name: F) {
        let old_name = self.get_field(&FIELD_NAME);
        let new_name = update_name(&old_name);
        self.set_field(&FIELD_NAME, &new_name);
    }

    pub fn join_entries(records: &[Record]) -> Record {
        let types: Vec<&RecordType> = records.iter().map(|e| e.record_type).collect();
        let record_type_name = common_type(&types);
        let mut result = record_type_name.new_record();

        for record in records.iter() {
            result.join(record);
        }

        let names: Vec<String> = records.iter().map(|p| p.name()).collect();
        result.set_field(&FIELD_NAME, &common_name(&names));
        result
    }

    pub fn join(&mut self, record: &Record) {
        let mut unmapped = Vec::new();
        for src_field in &record.record_type.fields {
            if src_field.name != FIELD_NAME.name && src_field.name != FIELD_DESCRIPTION.name {
                let value = record.get_field(src_field);
                if !value.is_empty() {
                    if let Some(dst_field) = self.record_type.fields.iter().find(|f| f.name == src_field.name) {
                        let dst_value = self.get_field(dst_field);
                        if dst_value.is_empty() {
                            self.set_field(dst_field, &value);
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
                unmapped.insert(0, &*FIELD_DESCRIPTION);
            }
        }

        if !unmapped.is_empty() {
            let mut desc = String::new();
            desc.push_str(&self.get_field(&FIELD_DESCRIPTION));

            if !desc.is_empty() {
                desc.push_str("\n\n");
            }
            desc.push_str(&record.get_field(&FIELD_NAME));
            desc.push_str("\n");
            desc.push_str("----------");
            desc.push_str("\n");
            for field in unmapped {
                desc.push_str(field.title);
                desc.push_str(": ");
                desc.push_str(&record.get_field(field));
                desc.push_str("\n");
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
