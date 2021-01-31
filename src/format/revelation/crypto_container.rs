use super::file_header::FileHeader;
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
use std::io::{Read, Write};

#[derive(Debug)]
pub enum CryptoError {
    UnknownFormat,
    WrongSize,
    CorruptedFile(String),
    DecryptError,
    Io(std::io::Error),
}

impl std::error::Error for CryptoError {}

impl std::fmt::Display for CryptoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CryptoError::UnknownFormat => write!(f, "Bad file (unknown format)"),
            CryptoError::WrongSize => write!(f, "Bad file (wrong size)"),
            CryptoError::CorruptedFile(_) => write!(f, "Bad file (corrupted)"),
            CryptoError::DecryptError => write!(f, "File cannot be decrypted"),
            CryptoError::Io(err) => write!(f, "I/O error: {}", err),
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

pub fn decrypt_file(source: &mut dyn Read, password: &str) -> Result<Vec<u8>, CryptoError> {
    let header = FileHeader::read(source).map_err(CryptoError::Io)?;

    if header.data_version != 1 {
        return Err(CryptoError::UnknownFormat);
    }

    let mut iv = GenericArray::<u8, U16>::default();
    source.read_exact(&mut iv).map_err(CryptoError::Io)?;

    let mut encrypted_content = Vec::new();
    source
        .read_to_end(&mut encrypted_content)
        .map_err(CryptoError::Io)?;
    if encrypted_content.len() % 16 != 0 {
        return Err(CryptoError::WrongSize);
    }

    let password = adjust_password(password).into();

    // decrypt the initial vector for CBC decryption
    Aes256::new(&password).decrypt_block(&mut iv);

    let decrypted = Cbc::<Aes256, Pkcs7>::new_fix(&password, &iv)
        .decrypt_vec(&encrypted_content)
        .map_err(|_| CryptoError::DecryptError)?;

    inflate_bytes_zlib(&decrypted).map_err(CryptoError::CorruptedFile)
}

pub fn encrypt_file(writer: &mut dyn Write, data: &[u8], password: &str) -> std::io::Result<()> {
    let password = adjust_password(password).into();

    let deflated = deflate_bytes_zlib(data);

    let mut iv = random::<[u8; 16]>().into();

    let encrypted = Cbc::<Aes256, Pkcs7>::new_fix(&password, &iv).encrypt_vec(&deflated);

    Aes256::new(&password).encrypt_block(&mut iv);

    let header = FileHeader {
        data_version: 1,
        app_version: (0, 4, 11),
    };

    header.write(writer)?;
    writer.write_all(&iv)?;
    writer.write_all(&encrypted)?;

    Ok(())
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
}
