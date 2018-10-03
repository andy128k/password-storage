use failure::Fail;

pub type Error = failure::Error;
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Fail, Debug)]
#[fail(display = "Bad file")]
pub struct BadFile;

#[derive(Fail, Debug)]
#[fail(display = "File cannot be encrypted")]
pub struct EncryptError;

#[derive(Fail, Debug)]
#[fail(display = "File cannot be decrypted")]
pub struct DecryptError;
