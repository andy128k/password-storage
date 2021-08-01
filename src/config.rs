use crate::error::*;
use crate::gtk_prelude::*;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Config {
    pub search_in_secrets: bool,
    pub show_secrets_on_preview: bool,
}

fn config_path() -> PathBuf {
    glib::user_config_dir().join("password-storage.toml")
}

impl Config {
    fn from_file(filename: &Path) -> Result<Self> {
        let buf = fs::read(filename)?;
        let config = toml::from_slice(&buf)?;
        Ok(config)
    }

    pub fn load() -> Self {
        let filename = config_path();
        Self::from_file(&filename).unwrap_or_else(|err| {
            eprintln!("{:?}", err);
            Default::default()
        })
    }

    pub fn save(&self) -> Result<()> {
        let filename = config_path();
        let dump = toml::to_vec(self)?;
        fs::write(&filename, &dump)?;
        Ok(())
    }
}
