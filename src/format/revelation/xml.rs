use lazy_static::lazy_static;
use minidom::{Node, Element};
use crate::error::*;
use crate::model::record::*;
use crate::model::tree::{RecordTree, RecordNode};

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
            ("password", "generic-password")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_CREDITCARD) {
        Some(&[
            ("cardtype", "creditcard-cardtype"),
            ("cardnumber", "creditcard-cardnumber"),
            ("expirydate", "creditcard-expirydate"),
            ("ccv", "creditcard-ccv"),
            ("pin", "generic-pin")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_CRYPTOKEY) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("certificate", "generic-certificate"),
            ("keyfile", "generic-keyfile"),
            ("password", "generic-password")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_DATABASE) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("username", "generic-username"),
            ("password", "generic-password"),
            ("database", "generic-database")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_DOOR) {
        Some(&[
            ("location", "generic-location"),
            ("code", "generic-code")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_EMAIL) {
        Some(&[
            ("email", "generic-email"),
            ("hostname", "generic-hostname"),
            ("username", "generic-username"),
            ("password", "generic-password")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_FTP) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("port", "generic-port"),
            ("username", "generic-username"),
            ("password", "generic-password")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_PHONE) {
        Some(&[
            ("phonenumber", "phone-phonenumber"),
            ("pin", "generic-pin")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_SHELL) {
        Some(&[
            ("hostname", "generic-hostname"),
            ("domain", "generic-domain"),
            ("username", "generic-username"),
            ("password", "generic-password")
        ])
    } else if record_type.ref_eq(&RECORD_TYPE_WEBSITE) {
        Some(&[
            ("url", "generic-url"),
            ("username", "generic-username"),
            ("password", "generic-password")
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
    let record_type = detect_type_by_xml_type(xml_type).ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;
    let mappings = get_fields_mapping(record_type).ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;

    let mut record = record_type.new_record();
    record.values.insert("name", &entry_node_get_value(xml_node, "name"));
    record.values.insert("description", &entry_node_get_value(xml_node, "description"));

    for &(ref field, ref id) in mappings {
        record.values.insert(field, &entry_field_by_id(xml_node, id));
    }

    Ok(record)
}

fn record_to_xml(record: &Record) -> Result<Element> {
    let xml_type = detect_xml_type_by_type(record.record_type).ok_or_else(|| format!("Bad entry type '{}'.", record.record_type.name))?;
    let mappings = get_fields_mapping(record.record_type).ok_or_else(|| format!("Bad entry type '{}'.", xml_type))?;

    let mut elem = Element::builder("entry").attr("type", xml_type);

    elem = elem.append(
        Element::builder("name").append(record.get_field(&FIELD_NAME)).build()
    );
    elem = elem.append(
        Element::builder("description").append(record.get_field(&FIELD_DESCRIPTION)).build()
    );
    for &(ref field, ref id) in mappings {
        elem = elem.append(
            Element::builder("field").attr("id", *id)
                .append(Node::Text(record.values.get(field).unwrap_or_default()))
                .build()
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

pub fn record_tree_from_xml(elem: &Element) -> Result<RecordTree> {
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

    fn read_records(elem: &Element) -> Result<RecordTree> {
        let children: Result<Vec<RecordNode>> = subentries(elem).iter().map(|e| read_record(e)).collect();
        Ok(Box::new(children?))
    }

    if elem.name() == "revelationdata" {
        read_records(elem)
    } else {
        Err("Bad xml element".into())
    }
}

pub fn record_tree_to_xml(tree: &RecordTree) -> Result<Element> {
    fn traverse(tree: &RecordTree, parent_element: &mut Element) -> Result<()> {
        for node in tree.iter() {
            match *node {
                RecordNode::Group(ref record, ref nodes) => {
                    let mut element = record_to_xml(record)?;
                    traverse(nodes, &mut element)?;
                    parent_element.append_child(element);
                },
                RecordNode::Leaf(ref record) => {
                    let element = record_to_xml(record)?;
                    parent_element.append_child(element);
                }
            }
        }
        Ok(())
    }

    let mut root = Element::builder("revelationdata").attr("version", "0.4.11").attr("dataversion", "1").build();
    traverse(tree, &mut root)?;
    Ok(root)
}
