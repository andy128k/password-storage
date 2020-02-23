mod crypto_container;
mod xml;

use std::path::Path;
use std::fs::{read, write};
use crate::model::tree::RecordTree;
use crate::error::*;

pub fn load_revelation_file(filename: &Path, password: &str) -> Result<RecordTree> {
    let buf = read(filename)?;
    let decrypted = crypto_container::decrypt_file(&buf, password)?;
    let string: String = String::from_utf8(decrypted)?;
    let element = string.parse().map_err(|e: minidom::error::Error| format_err!("{}", e))?;
    let tree = xml::record_tree_from_xml(&element)?;
    Ok(tree)
}

pub fn save_revelation_file(filename: &Path, password: &str, tree: &RecordTree) -> Result<()> {
    let element = xml::record_tree_to_xml(tree)?;

    let mut buf: Vec<u8> = Vec::new();
    element.write_to(&mut buf).map_err(|e| format_err!("{}", e))?;

    let file_dump = crypto_container::encrypt_file(&buf, password);
    write(filename, &file_dump)?;
    Ok(())
}
