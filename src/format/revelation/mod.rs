mod crypto_container;
mod file_header;
mod xml;

use crate::error::*;
use crate::model::tree::RecordTree;
use std::io::{Read, Write};

pub fn load_revelation_file(source: &mut dyn Read, password: &str) -> Result<RecordTree> {
    let decrypted = crypto_container::decrypt_file(source, password)?;
    let string: String = String::from_utf8(decrypted)?;
    let element = string.parse()?;
    let tree = xml::record_tree_from_xml(&element)?;
    Ok(tree)
}

pub fn save_revelation_file(
    destination: &mut dyn Write,
    password: &str,
    tree: &RecordTree,
) -> Result<()> {
    let element = xml::record_tree_to_xml(tree)?;

    let mut buf: Vec<u8> = Vec::new();
    element.write_to(&mut buf)?;

    crypto_container::encrypt_file(destination, &buf, password)?;

    Ok(())
}
