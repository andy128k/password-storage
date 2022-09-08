use crate::error::Result;
use crate::model::record::*;
use crate::model::tree::{RecordNode, RecordTree};
use crate::version::Version;
use lazy_static::lazy_static;
use quick_xml::{
    events::{attributes::Attributes, BytesDecl, BytesEnd, BytesStart, BytesText, Event},
    Reader, Writer,
};
use std::io::{BufRead, Write};

pub fn record_tree_from_xml(data: &[u8]) -> Result<RecordTree> {
    let mut reader = Reader::from_reader(data);
    let record_tree = read_document(&mut reader)?;
    Ok(record_tree)
}

fn read_document<R: BufRead>(reader: &mut Reader<R>) -> Result<RecordTree> {
    let mut buf = Vec::new();
    let mut record_tree = None;
    loop {
        buf.clear();
        match reader.read_event_into(&mut buf)? {
            Event::Decl(..) => {}
            Event::Start(ref e)
                if record_tree.is_none() && e.name().as_ref() == b"revelationdata" =>
            {
                record_tree = Some(read_revelationdata(reader, &mut e.attributes())?);
            }
            Event::Empty(ref e)
                if record_tree.is_none() && e.name().as_ref() == b"revelationdata" =>
            {
                record_tree = Some(vec![]);
            }
            Event::Text(element) if element.unescape()?.trim().is_empty() => {}
            Event::Eof => break,
            e => return Err(format!("1 Unexpected XML event {:?}.", e).into()),
        }
    }

    if let Some(record_tree) = record_tree {
        Ok(RecordTree {
            records: record_tree,
        })
    } else {
        Err("Bad xml element".into())
    }
}

fn read_revelationdata<R: BufRead>(
    reader: &mut Reader<R>,
    atts: &mut Attributes,
) -> Result<Vec<RecordNode>> {
    let _version: Option<Version> = read_attribute(reader, atts, b"version")?
        .map(|s| s.parse())
        .transpose()?;
    let _dataversion: Option<u8> = read_attribute(reader, atts, b"dataversion")?
        .map(|s| s.parse())
        .transpose()?;

    let mut records = Vec::new();
    let mut buf = Vec::new();
    loop {
        buf.clear();
        match reader.read_event_into(&mut buf)? {
            Event::Start(ref e) if e.name().as_ref() == b"entry" => {
                let record_node = read_record_node(reader, &mut e.attributes())?;
                records.push(record_node);
            }
            Event::Empty(ref e) if e.name().as_ref() == b"entry" => {
                let record_node = read_empty_record_node(reader, &mut e.attributes())?;
                records.push(record_node);
            }
            Event::Text(element) if element.unescape()?.trim().is_empty() => {}
            Event::End(ref e) if e.name().as_ref() == b"revelationdata" => break,
            e => return Err(format!("3 Unexpected XML event {:?}.", e).into()),
        }
    }
    Ok(records)
}

fn read_record_node<R: BufRead>(
    reader: &mut Reader<R>,
    atts: &mut Attributes,
) -> Result<RecordNode> {
    let xml_type = expect_attribute(reader, atts, "type")?;

    let mapping = KNOWN_TYPES
        .iter()
        .find(|mapping| mapping.xml_type_name == xml_type)
        .ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;

    let is_group = xml_type == "folder";

    let mut record = mapping.record_type.new_record();
    let mut children = Vec::new();
    let mut buf = Vec::new();
    loop {
        buf.clear();
        match reader.read_event_into(&mut buf)? {
            Event::Start(ref e) => match e.name().as_ref() {
                b"name" => {
                    let value = expect_text(reader, b"name")?;
                    record.values.insert("name".to_string(), value);
                }
                b"description" => {
                    let value = expect_text(reader, b"description")?;
                    record.values.insert("description".to_string(), value);
                }
                b"field" => {
                    let id = expect_attribute(reader, &mut e.attributes(), "id")?;
                    let field_name = mapping
                        .fields
                        .iter()
                        .filter_map(|(field_name, xml_id)| {
                            if *xml_id == id {
                                Some(field_name)
                            } else {
                                None
                            }
                        })
                        .next()
                        .ok_or_else(|| {
                            format!(
                                "Field {} is not expected in a record of type {}.",
                                id, xml_type
                            )
                        })?;
                    let value = expect_text(reader, b"field")?;
                    record.values.insert(field_name.to_string(), value);
                }
                b"entry" if is_group => {
                    let child = read_record_node(reader, &mut e.attributes())?;
                    children.push(child);
                }
                e => return Err(format!("Unexpected element {:?}.", e).into()),
            },
            Event::Empty(ref e) => match e.name().as_ref() {
                b"name" | b"description" | b"field" => {}
                b"entry" if is_group => {
                    let child = read_empty_record_node(reader, &mut e.attributes())?;
                    children.push(child);
                }
                e => return Err(format!("Unexpected element {:?}.", e).into()),
            },
            Event::Text(element) if element.unescape()?.trim().is_empty() => {}
            Event::End(ref e) if e.name().as_ref() == b"entry" => break,
            e => return Err(format!(" 5 Unexpected XML event {:?} {:?}.", e, record).into()),
        }
    }

    if is_group {
        Ok(RecordNode::Group(record, children))
    } else {
        Ok(RecordNode::Leaf(record))
    }
}

fn read_empty_record_node<R: BufRead>(
    reader: &mut Reader<R>,
    atts: &mut Attributes,
) -> Result<RecordNode> {
    let xml_type = expect_attribute(reader, atts, "type")?;

    let mapping = KNOWN_TYPES
        .iter()
        .find(|mapping| mapping.xml_type_name == xml_type)
        .ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;

    let is_group = xml_type == "folder";
    let record = mapping.record_type.new_record();
    if is_group {
        Ok(RecordNode::Group(record, vec![]))
    } else {
        Ok(RecordNode::Leaf(record))
    }
}

fn expect_text<R: BufRead>(reader: &mut Reader<R>, end_name: &[u8]) -> Result<String> {
    let mut result = String::new();
    let mut buf = Vec::new();
    loop {
        buf.clear();
        match reader.read_event_into(&mut buf)? {
            Event::Text(text) => result.push_str(&text.unescape()?),
            Event::End(end) if end.name().as_ref() == end_name => break,
            e => return Err(format!("Unexpected XML event {:?}.", e).into()),
        }
    }
    Ok(result)
}

fn read_attribute<R: BufRead>(
    reader: &mut Reader<R>,
    atts: &mut Attributes,
    name: &[u8],
) -> Result<Option<String>> {
    for attr in atts.with_checks(false) {
        let attr = attr?;
        if attr.key.as_ref() == name {
            let value = attr.decode_and_unescape_value(reader)?.to_string();
            return Ok(Some(value));
        }
    }
    Ok(None)
}

fn expect_attribute<R: BufRead>(
    reader: &mut Reader<R>,
    atts: &mut Attributes,
    name: &str,
) -> Result<String> {
    let value = read_attribute(reader, atts, name.as_bytes())?;
    let value = value.ok_or_else(|| format!("Attribute '{}' was not found.", name))?;
    Ok(value)
}

pub fn record_tree_to_xml(tree: &RecordTree, app_version: Version) -> Result<Vec<u8>> {
    let mut buffer = Vec::new();
    let mut writer = Writer::new(&mut buffer);

    writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("utf-8"), None)))?;
    writer.write_event(Event::Start({
        let mut element = BytesStart::new("revelationdata");
        element.push_attribute(("version", app_version.to_string().as_str()));
        element.push_attribute(("dataversion", "1"));
        element
    }))?;

    for node in &tree.records {
        write_record_node(&mut writer, node)?;
    }

    writer.write_event(Event::End(BytesEnd::new("revelationdata")))?;

    Ok(buffer)
}

fn write_record_node<W: Write>(writer: &mut Writer<W>, record_node: &RecordNode) -> Result<()> {
    let record = record_node.record();

    let mapping = KNOWN_TYPES
        .iter()
        .find(|mapping| RecordType::ref_eq(mapping.record_type, record.record_type))
        .unwrap();

    writer.write_event(Event::Start({
        let mut element = BytesStart::new("entry");
        element.push_attribute(("type", mapping.xml_type_name));
        element
    }))?;

    write_field(writer, "name", record.get_field(&FIELD_NAME))?;
    write_field(writer, "description", record.get_field(&FIELD_DESCRIPTION))?;
    for &(field, id) in mapping.fields {
        let value: &str = record.values.get(field).map_or("", |v| &**v);
        write_generic_field(writer, id, value)?;
    }

    for child_node in record_node.children() {
        write_record_node(writer, child_node)?;
    }

    writer.write_event(Event::End(BytesEnd::new("entry")))?;
    Ok(())
}

fn write_field<W: Write>(writer: &mut Writer<W>, name: &str, value: &str) -> Result<()> {
    writer.write_event(Event::Start(BytesStart::new(name)))?;
    writer.write_event(Event::Text(BytesText::new(value)))?;
    writer.write_event(Event::End(BytesEnd::new(name)))?;
    Ok(())
}

fn write_generic_field<W: Write>(writer: &mut Writer<W>, id: &str, value: &str) -> Result<()> {
    writer.write_event(Event::Start({
        let mut element = BytesStart::new("field");
        element.push_attribute(("id", id));
        element
    }))?;
    writer.write_event(Event::Text(BytesText::new(value)))?;
    writer.write_event(Event::End(BytesEnd::new("field")))?;
    Ok(())
}

struct TypeMapping<'a> {
    xml_type_name: &'a str,
    record_type: &'a RecordType,
    fields: &'a [(&'a str, &'a str)],
}

lazy_static! {
    static ref KNOWN_TYPES: Vec<TypeMapping<'static>> = vec![
        TypeMapping {
            xml_type_name: "folder",
            record_type: &RECORD_TYPE_GROUP,
            fields: &[]
        },
        TypeMapping {
            xml_type_name: "generic",
            record_type: &RECORD_TYPE_GENERIC,
            fields: &[
                ("hostname", "generic-hostname"),
                ("username", "generic-username"),
                ("password", "generic-password"),
            ]
        },
        TypeMapping {
            xml_type_name: "creditcard",
            record_type: &RECORD_TYPE_CREDITCARD,
            fields: &[
                ("cardtype", "creditcard-cardtype"),
                ("cardnumber", "creditcard-cardnumber"),
                ("expirydate", "creditcard-expirydate"),
                ("ccv", "creditcard-ccv"),
                ("pin", "generic-pin"),
            ]
        },
        TypeMapping {
            xml_type_name: "cryptokey",
            record_type: &RECORD_TYPE_CRYPTOKEY,
            fields: &[
                ("hostname", "generic-hostname"),
                ("certificate", "generic-certificate"),
                ("keyfile", "generic-keyfile"),
                ("password", "generic-password"),
            ]
        },
        TypeMapping {
            xml_type_name: "database",
            record_type: &RECORD_TYPE_DATABASE,
            fields: &[
                ("hostname", "generic-hostname"),
                ("username", "generic-username"),
                ("password", "generic-password"),
                ("database", "generic-database"),
            ]
        },
        TypeMapping {
            xml_type_name: "door",
            record_type: &RECORD_TYPE_DOOR,
            fields: &[("location", "generic-location"), ("code", "generic-code"),]
        },
        TypeMapping {
            xml_type_name: "email",
            record_type: &RECORD_TYPE_EMAIL,
            fields: &[
                ("email", "generic-email"),
                ("hostname", "generic-hostname"),
                ("username", "generic-username"),
                ("password", "generic-password"),
            ]
        },
        TypeMapping {
            xml_type_name: "ftp",
            record_type: &RECORD_TYPE_FTP,
            fields: &[
                ("hostname", "generic-hostname"),
                ("port", "generic-port"),
                ("username", "generic-username"),
                ("password", "generic-password"),
            ]
        },
        TypeMapping {
            xml_type_name: "phone",
            record_type: &RECORD_TYPE_PHONE,
            fields: &[("phonenumber", "phone-phonenumber"), ("pin", "generic-pin"),]
        },
        TypeMapping {
            xml_type_name: "shell",
            record_type: &RECORD_TYPE_SHELL,
            fields: &[
                ("hostname", "generic-hostname"),
                ("domain", "generic-domain"),
                ("username", "generic-username"),
                ("password", "generic-password"),
            ]
        },
        TypeMapping {
            xml_type_name: "website",
            record_type: &RECORD_TYPE_WEBSITE,
            fields: &[
                ("url", "generic-url"),
                ("username", "generic-username"),
                ("password", "generic-password"),
            ]
        },
    ];
}

#[cfg(test)]
mod test {
    use super::*;

    const EMPTY_XML: &[u8] = b"<?xml version='1.0' encoding='utf-8'?>\n<revelationdata version=\"0.4.11\" dataversion=\"1\"/>";

    trait RecordBuilder {
        fn set(self, field: &Field, value: &str) -> Self;
    }

    impl RecordBuilder for Record {
        fn set(mut self, field: &Field, value: &str) -> Self {
            self.set_field(field, value);
            self
        }
    }

    fn test_tree() -> RecordTree {
        RecordTree {
            records: vec![
                RecordNode::Group(
                    RECORD_TYPE_GROUP.new_record().set(&FIELD_NAME, "Group 1"),
                    vec![
                        RecordNode::Leaf(
                            RECORD_TYPE_WEBSITE
                                .new_record()
                                .set(&FIELD_NAME, "website 1")
                                .set(&FIELD_PASSWORD, "letmein"),
                        ),
                        RecordNode::Leaf(
                            RECORD_TYPE_WEBSITE
                                .new_record()
                                .set(&FIELD_NAME, "website 2")
                                .set(&FIELD_PASSWORD, "secret"),
                        ),
                    ],
                ),
                RecordNode::Group(
                    RECORD_TYPE_GROUP.new_record().set(&FIELD_NAME, "Group 2"),
                    vec![
                        RecordNode::Group(
                            RECORD_TYPE_GROUP
                                .new_record()
                                .set(&FIELD_NAME, "Subgroup 1"),
                            vec![
                                RecordNode::Leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 3"),
                                ),
                                RecordNode::Leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 4"),
                                ),
                            ],
                        ),
                        RecordNode::Group(
                            RECORD_TYPE_GROUP
                                .new_record()
                                .set(&FIELD_NAME, "Subgroup 2"),
                            vec![
                                RecordNode::Leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 5"),
                                ),
                                RecordNode::Leaf(
                                    RECORD_TYPE_WEBSITE
                                        .new_record()
                                        .set(&FIELD_NAME, "website 6"),
                                ),
                            ],
                        ),
                    ],
                ),
                RecordNode::Leaf(
                    RECORD_TYPE_GENERIC
                        .new_record()
                        .set(&FIELD_NAME, "generic entry"),
                ),
                RecordNode::Leaf(
                    RECORD_TYPE_WEBSITE
                        .new_record()
                        .set(&FIELD_NAME, "website 7"),
                ),
                RecordNode::Leaf(
                    RECORD_TYPE_WEBSITE
                        .new_record()
                        .set(&FIELD_NAME, "website 8"),
                ),
            ],
        }
    }

    fn test_xml() -> String {
        r##"<?xml version="1.0" encoding="utf-8"?>
        <revelationdata version="0.4.11" dataversion="1">
            <entry type="folder">
                <name>Group 1</name>
                <description></description>
                <entry type="website"><name>website 1</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password">letmein</field></entry>
                <entry type="website"><name>website 2</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password">secret</field></entry>
            </entry>
            <entry type="folder">
                <name>Group 2</name>
                <description></description>
                <entry type="folder">
                    <name>Subgroup 1</name>
                    <description></description>
                    <entry type="website"><name>website 3</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
                    <entry type="website"><name>website 4</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
                </entry>
                <entry type="folder">
                    <name>Subgroup 2</name>
                    <description></description>
                    <entry type="website"><name>website 5</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
                    <entry type="website"><name>website 6</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
                </entry>
            </entry>
            <entry type="generic"><name>generic entry</name><description></description><field id="generic-hostname"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
            <entry type="website"><name>website 7</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
            <entry type="website"><name>website 8</name><description></description><field id="generic-url"></field><field id="generic-username"></field><field id="generic-password"></field></entry>
        </revelationdata>"##.lines().map(|s| s.trim()).fold(String::new(), |a, b| a + b)
    }

    #[test]
    fn test_read_empty() {
        let tree = record_tree_from_xml(EMPTY_XML).unwrap();
        assert!(tree.records.is_empty());
    }

    #[test]
    fn test_write_empty() {
        let app_version = Version {
            major: 0,
            minor: 4,
            patch: 11,
        };
        let empty_tree = RecordTree { records: vec![] };
        let data = record_tree_to_xml(&empty_tree, app_version).unwrap();
        assert_eq!(
            std::str::from_utf8(&data).unwrap(),
            "<?xml version=\"1.0\" encoding=\"utf-8\"?><revelationdata version=\"0.4.11\" dataversion=\"1\"></revelationdata>"
        );
    }

    #[test]
    fn test_read_tree() {
        let tree = record_tree_from_xml(test_xml().as_bytes()).unwrap();
        assert_eq!(tree, test_tree());
    }

    #[test]
    fn test_write_tree() {
        let app_version = Version {
            major: 0,
            minor: 4,
            patch: 11,
        };
        let tree = test_tree();
        let data = record_tree_to_xml(&tree, app_version).unwrap();
        assert_eq!(std::str::from_utf8(&data).unwrap(), test_xml());
    }
}
