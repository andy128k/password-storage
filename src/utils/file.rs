use std::fs::File;
use std::path::PathBuf;
use std::io::{Read, Write};
use error::*;

pub fn read_file(filename: &PathBuf) -> Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    Ok(buf)
}

pub fn write_file(filename: &PathBuf, buf: &[u8]) -> Result<()> {
    let mut file = File::create(filename)?;
    file.write_all(buf)?;
    Ok(())
}
