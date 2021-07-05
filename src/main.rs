#![windows_subsystem = "windows"]

mod actions;
mod application;
mod cache;
mod config;
mod entropy;
mod error;
mod format;
mod icons;
mod main_window;
mod markup_builder;
mod model;
mod password;
mod store;
mod test;
mod ui;
mod utils;
mod version;

use gtk::prelude::*;

fn main() {
    let app = application::PSApplication::new();
    let code = app.run();
    std::process::exit(code);
}
