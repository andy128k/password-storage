use failure::err_msg;
use crypto::symmetriccipher::{Encryptor, Decryptor};
use crypto::aes::{ecb_encryptor, ecb_decryptor, cbc_encryptor, cbc_decryptor, KeySize};
use crypto::blockmodes::{NoPadding, PkcsPadding};
use crypto::buffer::{RefReadBuffer, RefWriteBuffer, BufferResult, WriteBuffer, ReadBuffer};
use inflate::inflate_bytes_zlib;
use deflate::deflate_bytes_zlib;
use crate::error::*;

fn adjust_password(password: &str) -> [u8; 32] {
    let bytes = password.as_bytes();
    let len = usize::min(32, bytes.len());
    let mut array = [0; 32];
    array[..len].copy_from_slice(&bytes[..len]);
    array
}

trait ProcessAll {
    fn process(&mut self, read_buffer: &mut RefReadBuffer<'_>, write_buffer: &mut RefWriteBuffer<'_>) -> Result<BufferResult>;

    fn process_all(&mut self, decrypted_data: &[u8]) -> Result<Vec<u8>> {
        let mut final_result = Vec::<u8>::new();
        let mut read_buffer = RefReadBuffer::new(decrypted_data);
        let mut buffer = [0; 4096];
        let mut write_buffer = RefWriteBuffer::new(&mut buffer);

        loop {
            let result = self.process(&mut read_buffer, &mut write_buffer)?;
            final_result.extend(write_buffer.take_read_buffer().take_remaining());
            match result {
                BufferResult::BufferUnderflow => break,
                BufferResult::BufferOverflow => { }
            }
        }

        Ok(final_result)
    }
}

impl ProcessAll for dyn Encryptor {
    fn process(&mut self, read_buffer: &mut RefReadBuffer<'_>, write_buffer: &mut RefWriteBuffer<'_>) -> Result<BufferResult> {
        let result = self.encrypt(read_buffer, write_buffer, true).map_err(EncryptError)?;
        Ok(result)
    }
}

impl ProcessAll for dyn Decryptor {
    fn process(&mut self, read_buffer: &mut RefReadBuffer<'_>, write_buffer: &mut RefWriteBuffer<'_>) -> Result<BufferResult> {
        let result = self.decrypt(read_buffer, write_buffer, true).map_err(DecryptError)?;
        Ok(result)
    }
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
        return Err(BadFile.into());
    }

    if buffer[0..MAGIC.len()] != MAGIC {
        return Err(BadFile.into());
    }

    // decrypt the initial vector for CBC decryption
    let mut iv: [u8; 16] = [0u8; 16];

    ecb_decryptor(KeySize::KeySize256, &password, NoPadding)
        .decrypt(
            &mut RefReadBuffer::new(&buffer[12..28]),
            &mut RefWriteBuffer::new(&mut iv),
            true
        )
        .map_err(DecryptError)?;

    let decrypted = cbc_decryptor(KeySize::KeySize256, &password, &iv, PkcsPadding)
        .process_all(&buffer[28..])?;

    inflate_bytes_zlib(&decrypted).map_err(err_msg)
}

pub fn encrypt_file(buffer: &[u8], password: &str) -> Result<Vec<u8>> {
    let password = adjust_password(password);

    let deflated = deflate_bytes_zlib(buffer);

    let iv: [u8; 16] = ::rand::random::<[u8; 16]>();

    let encrypted = cbc_encryptor(KeySize::KeySize256, &password, &iv, PkcsPadding)
        .process_all(&deflated)?;

    let mut encrypted_iv: [u8; 16] = [0u8; 16];
    ecb_encryptor(KeySize::KeySize256, &password, NoPadding)
        .encrypt(
            &mut RefReadBuffer::new(&iv),
            &mut RefWriteBuffer::new(&mut encrypted_iv),
            true
        )
        .map_err(EncryptError)?;

    let mut result = Vec::with_capacity(MAGIC.len() + encrypted_iv.len() + encrypted.len());
    result.extend_from_slice(&MAGIC);
    result.extend_from_slice(&encrypted_iv);
    result.extend_from_slice(&encrypted);

    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_adjust_password() {
        assert_eq!(
            adjust_password("secr3t"),
            [0x73,0x65,0x63,0x72,0x33,0x74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        );
        assert_eq!(
            adjust_password("secr3tsecr3tsecr3tsecr3tsecr3tsecr3tsecr3t"),
            [
                0x73,0x65,0x63,0x72,0x33,0x74,
                0x73,0x65,0x63,0x72,0x33,0x74,
                0x73,0x65,0x63,0x72,0x33,0x74,
                0x73,0x65,0x63,0x72,0x33,0x74,
                0x73,0x65,0x63,0x72,0x33,0x74,
                0x73,0x65
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
        let encrypted = encrypt_file(empty_xml(), password).unwrap();
        assert_eq!(108, encrypted.len());
        assert_eq!(114, encrypted[0]);
        let decrypted = decrypt_file(&encrypted, password).unwrap();
        assert_eq!(decrypted, empty_xml());
    }
}
