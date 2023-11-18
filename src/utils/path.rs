use std::ffi::OsString;
use std::path::{Path, PathBuf};

pub fn path_as_bytes(path: &Path) -> &[u8] {
    path.as_os_str().as_encoded_bytes()
}

pub fn path_from_bytes(bytes: Vec<u8>) -> PathBuf {
    PathBuf::from(unsafe { OsString::from_encoded_bytes_unchecked(bytes) })
}
