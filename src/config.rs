use std::path::PathBuf;
use failure::err_msg;
use serde::{Serialize, Deserialize};
use glib::get_user_config_dir;
use crate::error::*;
use crate::utils::result_ext::ResultLogExt;
use crate::utils::file::{read_file, write_file};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Config {
    pub search_in_secrets: bool,
    pub show_secrets_on_preview: bool,
}

fn config_path() -> Result<PathBuf> {
    if let Some(mut path) = get_user_config_dir() {
        path.push("password-storage.toml");
        Ok(path)
    } else {
        Err(err_msg("Path to config is not detected"))
    }
}

impl Config {
    fn from_file(filename: &PathBuf) -> Result<Self> {
        let buf = read_file(filename)?;
        let config = toml::from_slice(&buf)?;
        Ok(config)
    }

    pub fn load() -> Self {
        config_path()
            .and_then(|filename| Self::from_file(&filename))
            .ok_log()
            .unwrap_or_default()
    }

    pub fn save(&self) -> Result<()> {
        let filename = config_path()?;
        let dump = toml::to_vec(self)?;
        write_file(&filename, &dump)?;
        Ok(())
    }
}
