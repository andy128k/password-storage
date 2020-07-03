#![cfg(test)]

pub fn test_gtk_init() {
    use gtk::init;
    use std::sync::Once;

    static START: Once = Once::new();
    START.call_once(|| {
        init().unwrap();
    });
}
