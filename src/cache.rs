use std::path::PathBuf;
use failure::err_msg;
use glib::get_user_cache_dir;
use error::*;
use utils::result_ext::ResultLogExt;
use ptr::*;

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
    fn from_file(filename: &PathBuf) -> Result<CachePrivate> {
        let buf = ::utils::file::read_file(filename)?;
        let config: CachePrivate = ::toml::from_slice(&buf)?;
        Ok(config)
    }

    pub fn load() -> Self {
        Self::new(cache_path()
            .and_then(|filename| Self::from_file(&filename))
            .ok_log()
            .unwrap_or_default())
    }

    pub fn save(&self) -> Result<()> {
        let filename = cache_path()?;
        let dump = ::toml::to_vec(&*self.borrow())?;
        ::utils::file::write_file(&filename, &dump)?;
        Ok(())
    }

    pub fn add_file(&self, filename: &PathBuf) {
        let mut private = self.borrow_mut();
        private.recent_files.retain(|ref f| *f != filename);
        private.recent_files.insert(0, filename.clone());
    }
}
