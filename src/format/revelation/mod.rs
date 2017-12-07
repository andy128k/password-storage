mod crypto_container;
mod xml;

use std::path::PathBuf;
use failure::SyncFailure;
use model::tree::RecordTree;
use error::*;

pub fn load_revelation_file(filename: &PathBuf, password: &str) -> Result<RecordTree> {
    let buf = ::utils::file::read_file(filename)?;
    let decrypted = crypto_container::decrypt_file(&buf, password)?;
    let string: String = String::from_utf8(decrypted)?;
    let element = string.parse().map_err(SyncFailure::new)?;
    let tree = xml::record_tree_from_xml(&element)?;
    Ok(tree)
}

pub fn save_revelation_file(filename: &PathBuf, password: &str, tree: &RecordTree) -> Result<()> {
    let element = xml::record_tree_to_xml(tree)?;

    let mut buf: Vec<u8> = Vec::new();
    element.write_to(&mut buf).map_err(SyncFailure::new)?;

    let file_dump = crypto_container::encrypt_file(&buf, password)?;
    ::utils::file::write_file(filename, &file_dump)?;
    Ok(())
}
