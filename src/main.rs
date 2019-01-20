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
