mod crypto_container;
mod file_header;
mod utils;
mod xml;

use crate::error::*;
use crate::model::tree::RecordTree;
use crate::version::{Version, version};
use std::io::{Read, Write};

enum CryptoContainer {
    Aes256,
}

impl CryptoContainer {
    fn decrypt(&self, source: &mut dyn Read, password: &str) -> Result<Vec<u8>> {
        match self {
            Self::Aes256 => crypto_container::decrypt(source, password),
        }
    }

    fn encrypt(&self, writer: &mut dyn Write, data: &[u8], password: &str) -> Result<()> {
        match self {
            Self::Aes256 => crypto_container::encrypt(writer, data, password),
        }
    }
}

#[derive(Debug)]
pub enum SerializationFormat {
    Xml,
}

impl SerializationFormat {
    fn deserialize(&self, data: &[u8]) -> Result<RecordTree> {
        match self {
            Self::Xml => xml::record_tree_from_xml(data),
        }
    }

    fn serialize(&self, tree: &RecordTree, app_version: Version) -> Result<Vec<u8>> {
        match self {
            Self::Xml => xml::record_tree_to_xml(tree, app_version),
        }
    }
}

fn version_impl(version: u8) -> Option<(CryptoContainer, SerializationFormat)> {
    match version {
        1 => Some((CryptoContainer::Aes256, SerializationFormat::Xml)),
        _ => None,
    }
}

pub struct Decrypted {
    pub format: SerializationFormat,
    pub content: Vec<u8>,
}

pub fn decrypt_revelation_file(source: &mut dyn Read, password: &str) -> Result<Decrypted> {
    let header = file_header::FileHeader::read(source)?;
    let (container, format) = version_impl(header.data_version).ok_or_else(|| {
        format!(
            "Unsupported format (rvl {}). Try to use version {} or above.",
            header.data_version, header.app_version
        )
    })?;
    let content = container.decrypt(source, password)?;
    Ok(Decrypted { format, content })
}

pub fn load_revelation_file(source: &mut dyn Read, password: &str) -> Result<RecordTree> {
    let Decrypted { format, content } = decrypt_revelation_file(source, password)?;
    let tree = format.deserialize(&content)?;
    Ok(tree)
}

pub fn save_revelation_file(
    destination: &mut dyn Write,
    password: &str,
    tree: &RecordTree,
) -> Result<()> {
    let data_version = 1;
    let app_version = version();
    let (container, format) = version_impl(data_version).expect("Version 1 must be supported.");
    let header = file_header::FileHeader {
        data_version,
        app_version,
    };
    header.write(destination)?;
    let xml = format.serialize(tree, app_version)?;
    container.encrypt(destination, &xml, password)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    const EMPTY_RVL: &[u8] = include_bytes!("./fixtures/empty.rvl");

    fn empty_tree() -> RecordTree {
        RecordTree {
            records: Default::default(),
        }
    }

    #[test]
    fn test_decrypt() {
        let buf = load_revelation_file(&mut Cursor::new(EMPTY_RVL), "secr3t").unwrap();
        assert!(buf.records.is_empty());
    }

    #[test]
    fn test_decrypt_bad_password() {
        let buf = load_revelation_file(&mut Cursor::new(EMPTY_RVL), "iforgotpassword");
        assert!(buf.is_err());
        let err = buf.err().unwrap();
        assert_eq!(format!("{}", err), "File cannot be decrypted");
    }

    #[test]
    fn test_encrypt_and_decrypt_back() {
        let password = "qwerty123456";
        let mut encrypted = Vec::new();
        save_revelation_file(&mut encrypted, password, &empty_tree()).unwrap();
        assert!(encrypted.len() > 12);
        assert_eq!(encrypted[0], b'r');
        let decrypted = load_revelation_file(&mut Cursor::new(&encrypted), password).unwrap();
        assert!(decrypted.records.is_empty());
    }
}
