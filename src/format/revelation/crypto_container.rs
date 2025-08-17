use super::utils::adjust_password;
use crate::error::Result;
use aes::{
    Aes256,
    cipher::{
        BlockDecrypt, BlockDecryptMut, BlockEncrypt, BlockEncryptMut, BlockSizeUser, KeyInit,
        KeyIvInit,
        block_padding::Pkcs7,
        generic_array::{GenericArray, typenum::U16},
    },
};
use deflate::deflate_bytes_zlib;
use inflate::inflate_bytes_zlib;
use rand::random;
use std::io::{Read, Write};

pub fn decrypt(source: &mut dyn Read, password: &str) -> Result<Vec<u8>> {
    let mut iv = GenericArray::<u8, U16>::default();
    source.read_exact(&mut iv)?;

    let mut encrypted_content = Vec::new();
    source.read_to_end(&mut encrypted_content)?;

    let password = adjust_password(password).into();

    // decrypt the initial vector for CBC decryption
    Aes256::new(&password).decrypt_block(&mut iv);
    let decrypted = cbc::Decryptor::<Aes256>::new(&password, &iv)
        .decrypt_padded_mut::<Pkcs7>(&mut encrypted_content)
        .map_err(|_| "File cannot be decrypted")?;
    let data = inflate_bytes_zlib(decrypted).map_err(|e| format!("Corrupted file: {e}"))?;
    Ok(data)
}

pub fn encrypt(writer: &mut dyn Write, data: &[u8], password: &str) -> Result<()> {
    let password = adjust_password(password).into();
    let deflated = deflate_bytes_zlib(data);

    let mut buffer = vec![0_u8; deflated.len() + cbc::Encryptor::<Aes256>::block_size()];

    let mut iv = random::<[u8; 16]>().into();
    let encrypted = cbc::Encryptor::<Aes256>::new(&password, &iv)
        .encrypt_padded_b2b_mut::<Pkcs7>(&deflated, &mut buffer)
        .map_err(|_| "File cannot be encrypted")?;
    Aes256::new(&password).encrypt_block(&mut iv);
    writer.write_all(&iv)?;
    writer.write_all(encrypted)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

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
