use gtk4::gio;
use gtk4::prelude::*;
use password_storage::application::PSApplication;

pub fn test_app_create() {
    let app = PSApplication::default();
    assert!(gio::Icon::for_string("entry-generic").is_ok());
    app.quit();
}
