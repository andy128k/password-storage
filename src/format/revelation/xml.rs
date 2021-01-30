use crate::error::*;
use crate::model::record::*;
use crate::model::tree::{RecordNode, RecordTree};
use lazy_static::lazy_static;
use minidom::{Element, Node};

type TypeMapping = Vec<(&'static str, &'static RecordType)>;

lazy_static! {
    pub static ref TYPES: TypeMapping = vec![
        ("folder", &RECORD_TYPE_GROUP),
        ("generic", &RECORD_TYPE_GENERIC),
        ("creditcard", &RECORD_TYPE_CREDITCARD),
        ("cryptokey", &RECORD_TYPE_CRYPTOKEY),
        ("database", &RECORD_TYPE_DATABASE),
        ("door", &RECORD_TYPE_DOOR),
        ("email", &RECORD_TYPE_EMAIL),
        ("ftp", &RECORD_TYPE_FTP),
        ("phone", &RECORD_TYPE_PHONE),
        ("shell", &RECORD_TYPE_SHELL),
        ("website", &RECORD_TYPE_WEBSITE)
    ];
}

fn detect_type_by_xml_type(xml_type: &str) -> Option<&'static RecordType> {
    for &(xml_type_name, record_type) in TYPES.iter() {
        if xml_type_name == xml_type {
            return Some(record_type);
        }
    }
    None
}

fn detect_xml_type_by_type(record_type: &'static RecordType) -> Option<&'static str> {
    for &(xml_type_name, record_type_ref) in TYPES.iter() {
        if RecordType::ref_eq(record_type_ref, record_type) {
            return Some(xml_type_name);
        }
    }
    None
}

type Mapping = &'static [(&'static str, &'static str)];

fn get_fields_mapping(record_type: &'static RecordType) -> Option<Mapping> {
    if record_type.ref_eq(&RECORD_TYPE_GROUP) {
        Some(&[])
    } else if record_type.ref_eq(&RECORD_TYPE_GENERIC) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("username", "generic-username"),
            ("password", "generic-password"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_CREDITCARD) {
        Some(&[
            ("cardtype", "creditcard-cardtype"),
            ("cardnumber", "creditcard-cardnumber"),
            ("expirydate", "creditcard-expirydate"),
            ("ccv", "creditcard-ccv"),
            ("pin", "generic-pin"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_CRYPTOKEY) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("certificate", "generic-certificate"),
            ("keyfile", "generic-keyfile"),
            ("password", "generic-password"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_DATABASE) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("username", "generic-username"),
            ("password", "generic-password"),
            ("database", "generic-database"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_DOOR) {
        Some(&[("location", "generic-location"), ("code", "generic-code")])
    } else if record_type.ref_eq(&RECORD_TYPE_EMAIL) {
        Some(&[
            ("email", "generic-email"),
            ("hostname", "generic-hostname"),
            ("username", "generic-username"),
            ("password", "generic-password"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_FTP) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("port", "generic-port"),
            ("username", "generic-username"),
            ("password", "generic-password"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_PHONE) {
        Some(&[("phonenumber", "phone-phonenumber"), ("pin", "generic-pin")])
    } else if record_type.ref_eq(&RECORD_TYPE_SHELL) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("domain", "generic-domain"),
            ("username", "generic-username"),
            ("password", "generic-password"),
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_WEBSITE) {
        Some(&[
            ("url", "generic-url"),
            ("username", "generic-username"),
            ("password", "generic-password"),
        ])
    } else {
        None
    }
}

fn entry_node_get_value(xml_node: &Element, tag_name: &str) -> String {
    for node in xml_node.nodes() {
        if let Node::Element(ref ch) = *node {
            if ch.name() == tag_name {
                return ch.text();
            }
        }
    }
    String::new()
}

fn entry_field_by_id(xml_node: &Element, id: &str) -> String {
    for node in xml_node.nodes() {
        if let Node::Element(ref ch) = *node {
            if ch.name() == "field" && ch.attr("id") == Some(id) {
                return ch.text();
            }
        }
    }
    String::new()
}

fn record_from_xml(xml_type: &str, xml_node: &Element) -> Result<Record> {
    let record_type = detect_type_by_xml_type(xml_type)
        .ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;
    let mappings =
        get_fields_mapping(record_type).ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;

    let mut record = record_type.new_record();
    record
        .values
        .insert("name".to_string(), entry_node_get_value(xml_node, "name"));
    record.values.insert(
        "description".to_string(),
        entry_node_get_value(xml_node, "description"),
    );

    for &(ref field, ref id) in mappings {
        record
            .values
            .insert(field.to_string(), entry_field_by_id(xml_node, id));
    }

    Ok(record)
}

fn record_to_xml(record: &Record) -> Result<Element> {
    let xml_type = detect_xml_type_by_type(record.record_type)
        .ok_or_else(|| format!("Bad entry type '{}'.", record.record_type.name))?;
    let mappings = get_fields_mapping(record.record_type)
        .ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;

    let mut elem = Element::builder("entry").attr("type", xml_type);

    elem = elem.append(
        Element::builder("name")
            .append(record.get_field(&FIELD_NAME))
            .build(),
    );
    elem = elem.append(
        Element::builder("description")
            .append(record.get_field(&FIELD_DESCRIPTION))
            .build(),
    );
    for &(ref field, ref id) in mappings {
        elem = elem.append(
            Element::builder("field")
                .attr("id", *id)
                .append(Node::Text(
                    record.values.get(*field).cloned().unwrap_or_default(),
                ))
                .build(),
        );
    }

    Ok(elem.build())
}

fn subentries(elem: &Element) -> Vec<&Element> {
    let mut result = Vec::new();
    for node in elem.nodes() {
        if let Node::Element(ref ch) = *node {
            if ch.name() == "entry" {
                result.push(ch);
            }
        }
    }
    result
}

pub fn record_tree_from_xml(data: &[u8]) -> Result<RecordTree> {
    fn read_record(elem: &Element) -> Result<RecordNode> {
        if let Some(type_) = elem.attr("type") {
            let record = record_from_xml(type_, elem)?;
            if type_ == "folder" {
                let children = read_records(elem)?;
                Ok(RecordNode::Group(record, children))
            } else {
                Ok(RecordNode::Leaf(record))
            }
        } else {
            Err("Bad xml element".into())
        }
    }

    fn read_records(elem: &Element) -> Result<Vec<RecordNode>> {
        subentries(elem).iter().map(|e| read_record(e)).collect()
    }

    let elem: Element = std::str::from_utf8(data)?.parse()?;

    if elem.name() == "revelationdata" {
        let records = read_records(&elem)?;
        Ok(RecordTree { records })
    } else {
        Err("Bad xml element".into())
    }
}

pub fn record_tree_to_xml(tree: &RecordTree) -> Result<Vec<u8>> {
    fn traverse(tree: &[RecordNode], parent_element: &mut Element) -> Result<()> {
        for node in tree {
            match *node {
                RecordNode::Group(ref record, ref nodes) => {
                    let mut element = record_to_xml(record)?;
                    traverse(nodes, &mut element)?;
                    parent_element.append_child(element);
                }
                RecordNode::Leaf(ref record) => {
                    let element = record_to_xml(record)?;
                    parent_element.append_child(element);
                }
            }
        }
        Ok(())
    }

    let mut root = Element::builder("revelationdata")
        .attr("version", "0.4.11")
        .attr("dataversion", "1")
        .build();
    traverse(&tree.records, &mut root)?;

    let mut buf: Vec<u8> = Vec::new();
    root.write_to(&mut buf)?;

    Ok(buf)
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
        r##"<revelationdata dataversion="1" version="0.4.11">
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
        let empty_tree = RecordTree { records: vec![] };
        let data = record_tree_to_xml(&empty_tree).unwrap();
        assert_eq!(
            data,
            br#"<revelationdata dataversion="1" version="0.4.11"/>"#
        );
    }

    #[test]
    fn test_read_tree() {
        let tree = record_tree_from_xml(test_xml().as_bytes()).unwrap();
        assert_eq!(tree, test_tree());
    }

    #[test]
    fn test_write_tree() {
        let tree = test_tree();
        let data = record_tree_to_xml(&tree).unwrap();
        assert_eq!(std::str::from_utf8(&data).unwrap(), test_xml());
    }
}
