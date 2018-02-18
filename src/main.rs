#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate failure;
extern crate rand;
extern crate crypto;
extern crate deflate;
extern crate inflate;
extern crate minidom;
extern crate glib;
extern crate glib_sys;
extern crate gobject_sys;
extern crate gio;
extern crate gdk;
extern crate gtk;
extern crate gtk_sys;
extern crate libc;
extern crate debug_cell;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate toml;
extern crate bincode;

pub mod error;
pub mod icons;
pub mod password;
pub mod utils;
pub mod ptr;
pub mod markup_builder;
pub mod model;
pub mod actions;
pub mod config;
pub mod cache;
pub mod version;
pub mod entropy;
pub mod store;
pub mod ui;
pub mod application;
pub mod main_window;
pub mod format;

fn main() {
    let app = application::PSApplication::new_app();
    app.run();
}
