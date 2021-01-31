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
        assert_eq!(encrypted.len(), 12 + 7 * 16);
        assert_eq!(encrypted[0], 114);
        let decrypted = load_revelation_file(&mut Cursor::new(&encrypted), password).unwrap();
        assert_eq!(decrypted, empty_tree());
    }
}
