#![windows_subsystem = "windows"]

use gtk4::prelude::*;
use password_storage::application::PSApplication;
use std::process::Termination;

fn main() -> impl Termination {
    let app = PSApplication::default();
    app.run()
}
