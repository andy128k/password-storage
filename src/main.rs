#![windows_subsystem = "windows"]

mod application;
mod cache;
mod config;
mod css;
mod entropy;
mod error;
mod format;
mod gtk_prelude;
mod id;
mod main_window;
mod markup_builder;
mod model;
mod password;
mod slot;
mod test;
mod ui;
mod utils;
mod version;
mod weak_map;

use crate::gtk_prelude::*;

fn main() {
    let app = application::PSApplication::default();
    let code = app.run();
    std::process::exit(code);
}
