#![cfg(test)]

pub fn test_gtk_init() {
    use std::sync::Once;

    static START: Once = Once::new();
    START.call_once(|| {
        gtk::init().unwrap();
    });
}
