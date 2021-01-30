mod crypto_container;
mod file_header;
mod xml;

use crate::error::*;
use crate::model::tree::RecordTree;
use std::io::{Read, Write};

pub fn load_revelation_file(source: &mut dyn Read, password: &str) -> Result<RecordTree> {
    let decrypted = crypto_container::decrypt_file(source, password)?;
    let tree = xml::record_tree_from_xml(&decrypted)?;
    Ok(tree)
}

pub fn save_revelation_file(
    destination: &mut dyn Write,
    password: &str,
    tree: &RecordTree,
) -> Result<()> {
    let xml = xml::record_tree_to_xml(tree)?;
    crypto_container::encrypt_file(destination, &xml, password)?;
    Ok(())
}
