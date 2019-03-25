mod error;
mod icons;
mod password;
mod utils;
mod ptr;
mod markup_builder;
mod model;
mod actions;
mod config;
mod cache;
mod version;
mod entropy;
mod store;
mod ui;
mod application;
mod main_window;
mod format;
mod test;

fn main() {
    let app = application::PSApplication::new_app();
    app.run();
}
