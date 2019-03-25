#![cfg(test)]

pub fn test_gtk_init() {
    use std::sync::Once;
    use gtk::init;

    static START: Once = Once::new();
    START.call_once(|| {
        init().unwrap();
    });
}
