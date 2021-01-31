mod crypto_container;
mod file_header;
mod xml;

use crate::error::*;
use crate::model::tree::RecordTree;
use crate::version::VERSION_PARSED;
use std::io::{Read, Write};

pub fn load_revelation_file(source: &mut dyn Read, password: &str) -> Result<RecordTree> {
    let header = file_header::FileHeader::read(source)?;
    let decrypted = match header.data_version {
        1 => crypto_container::decrypt(source, password)?,
        version => {
            return Err(format!(
                "Unsupported format (rvl {}). Try to use version {} or above.",
                version, header.app_version
            )
            .into());
        }
    };
    let tree = xml::record_tree_from_xml(&decrypted)?;
    Ok(tree)
}

pub fn save_revelation_file(
    destination: &mut dyn Write,
    password: &str,
    tree: &RecordTree,
) -> Result<()> {
    let header = file_header::FileHeader {
        data_version: 1,
        app_version: *VERSION_PARSED,
    };
    header.write(destination)?;

    let xml = xml::record_tree_to_xml(tree, *VERSION_PARSED)?;
    crypto_container::encrypt(destination, &xml, password)?;

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    const EMPTY_RVL: &[u8] = include_bytes!("./fixtures/empty.rvl");

    fn empty_tree() -> RecordTree {
        RecordTree {
            records: Vec::new(),
        }
    }

    #[test]
    fn test_decrypt() {
        let buf = load_revelation_file(&mut Cursor::new(EMPTY_RVL), "secr3t").unwrap();
        assert_eq!(buf, empty_tree());
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
        assert_eq!(decrypted, empty_tree());
    }
}
