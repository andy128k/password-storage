mod crypto_container;
mod xml;

use crate::error::*;
use crate::model::tree::RecordTree;
use std::io::{Read, Write};

pub fn load_revelation_file(source: &mut dyn Read, password: &str) -> Result<RecordTree> {
    let mut buf = Vec::new();
    source.read_to_end(&mut buf)?;
    let decrypted = crypto_container::decrypt_file(&buf, password)?;
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

    let file_dump = crypto_container::encrypt_file(&buf, password);
    destination.write_all(&file_dump)?;
    Ok(())
}
