use crate::error::*;
use gtk::glib;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::fs::{read, write};
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[derive(Debug, Default, Serialize, Deserialize)]
struct CachePrivate {
    pub recent_files: Vec<PathBuf>,
}

#[derive(Clone)]
pub struct Cache(Rc<RefCell<CachePrivate>>);

fn cache_path() -> PathBuf {
    glib::user_cache_dir().join("password-storage.toml")
}

impl Cache {
    fn from_file(filename: &Path) -> Result<CachePrivate> {
        let buf = read(filename)?;
        let config: CachePrivate = toml::from_slice(&buf)?;
        Ok(config)
    }

    pub fn load() -> Self {
        let filename = cache_path();
        Self(Rc::new(RefCell::new(
            Self::from_file(&filename).unwrap_or_else(|err| {
                eprintln!("{:?}", err);
                Default::default()
            }),
        )))
    }

    pub fn save(&self) -> Result<()> {
        let filename = cache_path();
        let dump = toml::to_vec(&*self.0.borrow())?;
        write(&filename, &dump)?;
        Ok(())
    }

    pub fn add_file(&self, filename: &Path) {
        let mut private = self.0.borrow_mut();
        private.recent_files.retain(|f| f != filename);
        private.recent_files.insert(0, filename.to_owned());
    }

    pub fn remove_file(&self, filename: &Path) {
        let mut private = self.0.borrow_mut();
        private.recent_files.retain(|f| f != filename);
    }

    pub fn recent_files(&self) -> Vec<PathBuf> {
        self.0.borrow().recent_files.clone()
    }
}
