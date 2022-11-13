mod crypto_container;
mod file_header;
mod utils;
mod xml;

use crate::error::*;
use crate::model::tree::RecordTree;
use crate::version::{Version, VERSION_PARSED};
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

enum SerializationFormat {
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

pub fn load_revelation_file(source: &mut dyn Read, password: &str) -> Result<RecordTree> {
    let header = file_header::FileHeader::read(source)?;
    let (container, format) = version_impl(header.data_version).ok_or_else(|| {
        format!(
            "Unsupported format (rvl {}). Try to use version {} or above.",
            header.data_version, header.app_version
        )
    })?;
    let decrypted = container.decrypt(source, password)?;
    let tree = format.deserialize(&decrypted)?;
    Ok(tree)
}

pub fn save_revelation_file(
    destination: &mut dyn Write,
    password: &str,
    tree: &RecordTree,
) -> Result<()> {
    let version = 1;
    let (container, format) = version_impl(version).expect("Version 1 must be supported.");
    let header = file_header::FileHeader {
        data_version: version,
        app_version: *VERSION_PARSED,
    };
    header.write(destination)?;
    let xml = format.serialize(tree, *VERSION_PARSED)?;
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
