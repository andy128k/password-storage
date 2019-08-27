use std::path::{Path, PathBuf};
use std::fs::{read, write};
use failure::err_msg;
use serde::{Serialize, Deserialize};
use glib::get_user_cache_dir;
use crate::error::*;
use crate::ptr::*;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct CachePrivate {
    pub recent_files: Vec<PathBuf>
}

pub type Cache = SharedPtr<CachePrivate>;

fn cache_path() -> Result<PathBuf> {
    if let Some(mut path) = get_user_cache_dir() {
        path.push("password-storage.toml");
        Ok(path)
    } else {
        Err(err_msg("Path to cache is not detected"))
    }
}

impl Cache {
    fn from_file(filename: &Path) -> Result<CachePrivate> {
        let buf = read(filename)?;
        let config: CachePrivate = toml::from_slice(&buf)?;
        Ok(config)
    }

    pub fn load() -> Self {
        Self::from_private(
            cache_path()
                .and_then(|filename| Self::from_file(&filename))
                .unwrap_or_else(|err| {
                    println!("{:?}", err);
                    Default::default()
                })
        )
    }

    pub fn save(&self) -> Result<()> {
        let filename = cache_path()?;
        let dump = toml::to_vec(&*self.borrow())?;
        write(&filename, &dump)?;
        Ok(())
    }

    pub fn add_file(&self, filename: &Path) {
        let mut private = self.borrow_mut();
        private.recent_files.retain(|f| f != filename);
        private.recent_files.insert(0, filename.to_owned());
    }
}
