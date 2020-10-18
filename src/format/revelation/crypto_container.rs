use aes::{
    cipher::block::{
        generic_array::{typenum::U16, GenericArray},
        BlockCipher, NewBlockCipher,
    },
    Aes256,
};
use block_modes::{block_padding::Pkcs7, BlockMode, Cbc};
use deflate::deflate_bytes_zlib;
use inflate::inflate_bytes_zlib;
use rand::random;

#[derive(Debug)]
pub enum CryptoError {
    UnknownFormat,
    WrongSize,
    CorruptedFile(String),
    DecryptError,
}

impl std::error::Error for CryptoError {}

impl std::fmt::Display for CryptoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CryptoError::UnknownFormat => write!(f, "Bad file (unknown format)"),
            CryptoError::WrongSize => write!(f, "Bad file (wrong size)"),
            CryptoError::CorruptedFile(_) => write!(f, "Bad file (corrupted)"),
            CryptoError::DecryptError => write!(f, "File cannot be decrypted"),
        }
    }
}

fn adjust_password(password: &str) -> [u8; 32] {
    let bytes = password.as_bytes();
    let len = usize::min(32, bytes.len());
    let mut array = [0; 32];
    array[..len].copy_from_slice(&bytes[..len]);
    array
}

#[rustfmt::skip]
const MAGIC: [u8; 12] = [
    0x72, 0x76, 0x6C, 0x00, // magic string
    0x01,                   // data version
    0x00,                   // separator
    0x00, 0x04, 0x0B,       // app version
    0x00, 0x00, 0x00        // separator
];

pub fn decrypt_file(buffer: &[u8], password: &str) -> Result<Vec<u8>, CryptoError> {
    if buffer.len() < 28 || (buffer.len() - 28) % 16 != 0 {
        return Err(CryptoError::WrongSize);
    }

    if buffer[0..MAGIC.len()] != MAGIC {
        return Err(CryptoError::UnknownFormat);
    }

    let password = adjust_password(password).into();

    // decrypt the initial vector for CBC decryption
    let mut iv = GenericArray::<u8, U16>::clone_from_slice(&buffer[12..28]);
    Aes256::new(&password).decrypt_block(&mut iv);

    let decrypted = Cbc::<Aes256, Pkcs7>::new_fix(&password, &iv)
        .decrypt_vec(&buffer[28..])
        .map_err(|_| CryptoError::DecryptError)?;

    inflate_bytes_zlib(&decrypted).map_err(CryptoError::CorruptedFile)
}

pub fn encrypt_file(buffer: &[u8], password: &str) -> Vec<u8> {
    let password = adjust_password(password).into();

    let deflated = deflate_bytes_zlib(buffer);

    let mut iv = random::<[u8; 16]>().into();

    let encrypted = Cbc::<Aes256, Pkcs7>::new_fix(&password, &iv).encrypt_vec(&deflated);

    Aes256::new(&password).encrypt_block(&mut iv);

    let mut result = Vec::with_capacity(MAGIC.len() + iv.len() + encrypted.len());
    result.extend_from_slice(&MAGIC);
    result.extend_from_slice(&iv);
    result.extend_from_slice(&encrypted);

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_adjust_password() {
        assert_eq!(
            adjust_password("secr3t"),
            [
                0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        assert_eq!(
            adjust_password("secr3tsecr3tsecr3tsecr3tsecr3tsecr3tsecr3t"),
            [
                0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0x73, 0x65,
                0x63, 0x72, 0x33, 0x74, 0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0x73, 0x65, 0x63, 0x72,
                0x33, 0x74, 0x73, 0x65
            ]
        );
    }

    const EMPTY_RVL: &[u8] = include_bytes!("./fixtures/empty.rvl");

    fn empty_xml() -> &'static [u8] {
        "<?xml version='1.0' encoding='utf-8'?>\n<revelationdata version=\"0.4.11\" dataversion=\"1\"/>".as_bytes()
    }

    #[test]
    fn test_decrypt() {
        let buf = decrypt_file(EMPTY_RVL, "secr3t").unwrap();
        assert_eq!(buf, empty_xml());
    }

    #[test]
    fn test_decrypt_bad_password() {
        let buf = decrypt_file(EMPTY_RVL, "iforgotpassword");
        assert!(buf.is_err());
        let err = buf.err().unwrap();
        assert_eq!(format!("{}", err), "File cannot be decrypted");
    }

    #[test]
    fn test_encrypt_and_decrypt_back() {
        let password = "qwerty123456";
        let encrypted = encrypt_file(empty_xml(), password);
        assert_eq!(108, encrypted.len());
        assert_eq!(114, encrypted[0]);
        let decrypted = decrypt_file(&encrypted, password).unwrap();
        assert_eq!(decrypted, empty_xml());
    }
}
