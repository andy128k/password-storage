use crypto::symmetriccipher::{Encryptor, Decryptor};
use crypto::aes::{ecb_encryptor, ecb_decryptor, cbc_encryptor, cbc_decryptor, KeySize};
use crypto::blockmodes::{NoPadding, PkcsPadding};
use crypto::buffer::{RefReadBuffer, RefWriteBuffer, BufferResult, WriteBuffer, ReadBuffer};
use inflate::inflate_bytes_zlib;
use deflate::deflate_bytes_zlib;
use error::*;

fn adjust_password(password: &str) -> Vec<u8> {
    let mut bytes: Vec<u8> = password.as_bytes().to_vec();
    bytes.resize(32, 0);
    bytes
}

fn encrypt_all(decrypted_data: &[u8], encryptor: &mut Box<Encryptor>) -> Result<Vec<u8>> {
    let mut final_result = Vec::<u8>::new();
    let mut read_buffer = RefReadBuffer::new(decrypted_data);
    let mut buffer = [0; 4096];
    let mut write_buffer = RefWriteBuffer::new(&mut buffer);

    loop {
        let result = encryptor.encrypt(&mut read_buffer, &mut write_buffer, true).map_err(ErrorKind::Crypto)?;
        final_result.extend(write_buffer.take_read_buffer().take_remaining());
        match result {
            BufferResult::BufferUnderflow => break,
            BufferResult::BufferOverflow => { }
        }
    }

    Ok(final_result)
}

fn decrypt_all(encrypted_data: &[u8], decryptor: &mut Box<Decryptor>) -> Result<Vec<u8>> {
    let mut final_result = Vec::<u8>::new();
    let mut read_buffer = RefReadBuffer::new(encrypted_data);
    let mut buffer = [0; 4096];
    let mut write_buffer = RefWriteBuffer::new(&mut buffer);

    loop {
        let result = decryptor.decrypt(&mut read_buffer, &mut write_buffer, true).map_err(ErrorKind::Crypto)?;
        final_result.extend(write_buffer.take_read_buffer().take_remaining());
        match result {
            BufferResult::BufferUnderflow => break,
            BufferResult::BufferOverflow => { }
        }
    }

    Ok(final_result)
}

const MAGIC: [u8; 12] = [
    0x72, 0x76, 0x6C, 0x00, // magic string
    0x01,                   // data version
    0x00,                   // separator
    0x00, 0x04, 0x0B,       // app version
    0x00, 0x00, 0x00        // separator
];

pub fn decrypt_file(buffer: &[u8], password: &str) -> Result<Vec<u8>> {
    let password = adjust_password(password);

    if buffer.len() < 28 || (buffer.len() - 28) % 16 != 0 {
        return Err(ErrorKind::BadFile.into());
    }

    if buffer[0..MAGIC.len()] != MAGIC {
        return Err(ErrorKind::BadFile.into());
    }

    // decrypt the initial vector for CBC decryption
    let mut iv: [u8; 16] = [0u8; 16];

    ecb_decryptor(KeySize::KeySize256, &password, NoPadding)
        .decrypt(
            &mut RefReadBuffer::new(&buffer[12..28]),
            &mut RefWriteBuffer::new(&mut iv),
            true
        )
        .map_err(ErrorKind::Crypto)?;

    let mut body_decryptor = cbc_decryptor(KeySize::KeySize256, &password, &iv, PkcsPadding);
    let decrypted = decrypt_all(&buffer[28..], &mut body_decryptor)?;

    inflate_bytes_zlib(&decrypted).map_err(|e| e.into())
}

pub fn encrypt_file(buffer: &[u8], password: &str) -> Result<Vec<u8>> {
    let password = adjust_password(password);

    let deflated = deflate_bytes_zlib(buffer);

    let iv: [u8; 16] = ::rand::random::<[u8; 16]>();

    let mut body_encryptor = cbc_encryptor(KeySize::KeySize256, &password, &iv, PkcsPadding);
    let encrypted = encrypt_all(&deflated, &mut body_encryptor)?;

    let mut encrypted_iv: [u8; 16] = [0u8; 16];
    ecb_encryptor(KeySize::KeySize256, &password, NoPadding)
        .encrypt(
            &mut RefReadBuffer::new(&iv),
            &mut RefWriteBuffer::new(&mut encrypted_iv),
            true
        )
        .map_err(ErrorKind::Crypto)?;

    let mut result = Vec::with_capacity(MAGIC.len() + encrypted_iv.len() + encrypted.len());
    result.extend_from_slice(&MAGIC);
    result.extend_from_slice(&encrypted_iv);
    result.extend_from_slice(&encrypted);

    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use error_chain::ChainedError;

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
        assert_eq!(err.description(), "Crypto error");
        assert_eq!(format!("{}", err.display()), "Error: Crypto error: InvalidPadding\n");
    }

    #[test]
    fn test_encrypt_and_decrypt_back() {
        let password = "qwerty123456";
        let encrypted = encrypt_file(empty_xml(), password).unwrap();
        assert_eq!(108, encrypted.len());
        assert_eq!(114, encrypted[0]);
        let decrypted = decrypt_file(&encrypted, password).unwrap();
        assert_eq!(decrypted, empty_xml());
    }
}
