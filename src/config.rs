use crate::error::*;
use crate::gtk_prelude::*;
use crate::id::Id;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[derive(Serialize, Deserialize, Default, Clone)]
pub struct Config {
    pub search_in_secrets: bool,
    pub show_secrets_on_preview: bool,
}

impl Config {
    fn from_file(filename: &Path) -> Result<Self> {
        let buf = fs::read(filename)?;
        let dump = toml::from_slice(&buf)?;
        Ok(dump)
    }

    pub fn to_file(&self, filename: &Path) -> Result<()> {
        let dump = toml::to_vec(self)?;
        fs::write(&filename, &dump)?;
        Ok(())
    }
}

pub struct ConfigService {
    id: Id,
    path: PathBuf,
    listeners: RefCell<HashMap<Id, Rc<dyn Fn(&Config)>>>,
    cached: RefCell<Option<Config>>,
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
            id: Default::default(),
            path,
            listeners: Default::default(),
            cached: Default::default(),
        }
    }

    pub fn get(&self) -> Config {
        if let Some(ref value) = *self.cached.borrow() {
            return value.clone();
        }
        let config = Config::from_file(&self.path).unwrap_or_else(|err| {
            eprintln!("Cannot load config: {:?}", err);
            Default::default()
        });
        *self.cached.borrow_mut() = Some(config.clone());
        config
    }

    pub fn set(&self, new_value: Config) {
        if let Err(err) = new_value.to_file(&self.path) {
            eprintln!("Cannot save config: {:?}", err);
        }
        for listener in self.listeners.borrow().values().cloned() {
            (listener)(&new_value);
        }
        *self.cached.borrow_mut() = Some(new_value);
    }

    pub fn subscribe(&self, listener: impl Fn(&Config) + 'static) -> ConfigListenerId {
        let listener_id = Id::default();
        self.listeners
            .borrow_mut()
            .insert(listener_id, Rc::new(listener));
        ConfigListenerId(self.id, listener_id)
    }

    pub fn unsubscribe(&self, listener_id: ConfigListenerId) {
        if listener_id.0 == self.id {
            self.listeners.borrow_mut().remove(&listener_id.1);
        } else {
            eprintln!("WARN cannot unsubscribe listener");
        }
    }
}
