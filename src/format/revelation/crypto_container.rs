use crate::error::Result;
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

fn adjust_password(password: &str) -> [u8; 32] {
    let bytes = password.as_bytes();
    let len = usize::min(32, bytes.len());
    let mut array = [0; 32];
    array[..len].copy_from_slice(&bytes[..len]);
    array
}

type Cipher = Cbc<Aes256, Pkcs7>;

pub fn decrypt(source: &mut dyn Read, password: &str) -> Result<Vec<u8>> {
    let mut iv = GenericArray::<u8, U16>::default();
    source.read_exact(&mut iv)?;

    let mut encrypted_content = Vec::new();
    source.read_to_end(&mut encrypted_content)?;
    if encrypted_content.len() % 16 != 0 {
        return Err(format!("Corrupted file: wrong size").into());
    }

    let password = adjust_password(password).into();

    // decrypt the initial vector for CBC decryption
    Aes256::new(&password).decrypt_block(&mut iv);
    let decrypted = Cipher::new_fix(&password, &iv)
        .decrypt_vec(&encrypted_content)
        .map_err(|_| "File cannot be decrypted")?;
    let data = inflate_bytes_zlib(&decrypted).map_err(|e| format!("Corrupted file: {}", e))?;
    Ok(data)
}

pub fn encrypt(writer: &mut dyn Write, data: &[u8], password: &str) -> Result<()> {
    let password = adjust_password(password).into();
    let deflated = deflate_bytes_zlib(data);
    let mut iv = random::<[u8; 16]>().into();
    let encrypted = Cipher::new_fix(&password, &iv).encrypt_vec(&deflated);
    Aes256::new(&password).encrypt_block(&mut iv);
    writer.write_all(&iv)?;
    writer.write_all(&encrypted)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

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

    #[test]
    fn test_encrypt_and_decrypt_back() {
        let password = "qwerty123456";
        let data = b"Hello, World!";
        let mut encrypted = Vec::new();
        encrypt(&mut encrypted, data, password).unwrap();
        let decrypted = decrypt(&mut Cursor::new(&encrypted), password).unwrap();
        assert_eq!(decrypted, data);
    }
}
