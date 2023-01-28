#![windows_subsystem = "windows"]

use gtk4::prelude::*;
use password_storage::application::PSApplication;

fn main() {
    let app = PSApplication::default();
    let code = app.run();
    std::process::exit(code);
}
