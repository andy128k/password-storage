use crate::error::*;
use crate::gtk_prelude::*;
use crate::id::Id;
use crate::slot::Slot;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Serialize, Deserialize, Default, Clone, Copy)]
pub struct Config {
    pub search_in_secrets: bool,
}

impl Config {
    fn load_from_file(filename: &Path) -> Result<Self> {
        let buf = fs::read(filename)?;
        let dump = toml::from_slice(&buf)?;
        Ok(dump)
    }

    pub fn save_to_file(&self, filename: &Path) -> Result<()> {
        let dump = toml::to_vec(self)?;
        fs::write(filename, dump)?;
        Ok(())
    }
}

pub struct ConfigService {
    path: PathBuf,
    cached: RefCell<Option<Config>>,
    pub on_change: Slot<Config>,
}

#[derive(Clone, Copy)]
pub struct ConfigListenerId(Id, Id);

impl std::default::Default for ConfigService {
    fn default() -> Self {
        Self::new(glib::user_config_dir().join("password-storage.toml"))
    }
}

impl ConfigService {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            cached: Default::default(),
            on_change: Default::default(),
        }
    }

    pub fn get(&self) -> Config {
        if let Some(ref value) = *self.cached.borrow() {
            return *value;
        }
        let config = Config::load_from_file(&self.path).unwrap_or_else(|err| {
            eprintln!("Cannot load config: {err:?}");
            Default::default()
        });
        *self.cached.borrow_mut() = Some(config);
        config
    }

    pub fn set(&self, new_value: Config) {
        if let Err(err) = new_value.save_to_file(&self.path) {
            eprintln!("Cannot save config: {err:?}");
        }
        self.on_change.emit(new_value);
        *self.cached.borrow_mut() = Some(new_value);
    }

    pub fn update(&self, updater: impl Fn(&mut Config)) {
        let mut value = self.get();
        (updater)(&mut value);
        self.set(value);
    }
}
