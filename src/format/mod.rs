pub mod revelation;

use crate::error::*;
use crate::model::tree::RecordTree;
use std::fs::OpenOptions;
use std::io::{BufReader, BufWriter};
use std::path::Path;

pub fn load_file(filename: &Path, password: &str) -> Result<RecordTree> {
    let file = OpenOptions::new().read(true).open(filename)?;
    let mut buffer = BufReader::new(file);
    let tree = revelation::load_revelation_file(&mut buffer, password)?;
    Ok(tree)
}

pub fn save_file(filename: &Path, password: &str, tree: &RecordTree) -> Result<()> {
    let file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(filename)?;
    let mut buffer = BufWriter::new(file);
    revelation::save_revelation_file(&mut buffer, password, tree)?;
    Ok(())
}
