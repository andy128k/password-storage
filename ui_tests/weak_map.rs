use gtk4 as gtk;
use gtk4::glib;
use password_storage::weak_map::WeakMap;

pub fn test_weak_map() {
    let key1 = "key1";
    let key2 = "key2";
    let widget1: gtk::Label = glib::Object::builder().build();
    let widget2: gtk::Label = glib::Object::builder().build();

    let map = WeakMap::<&'static str, gtk::Label>::default();

    map.add(key1, &widget2);
    map.add(key2, &widget2);
    map.add(key1, &widget1); // replace

    assert_eq!(map.find(key1).as_ref(), Some(&widget1));
    assert_eq!(map.find(key2).as_ref(), Some(&widget2));

    drop(widget1);

    assert_eq!(map.find(key1), None);
    assert_eq!(map.find(key2).as_ref(), Some(&widget2));

    drop(widget2);

    assert_eq!(map.find(key1), None);
    assert_eq!(map.find(key2), None);
}

pub fn test_weak_map_remove() {
    let key1 = "key1";
    let key2 = "key2";
    let widget1: gtk::Label = glib::Object::builder().build();
    let widget2: gtk::Label = glib::Object::builder().build();

    let map = WeakMap::<&'static str, gtk::Label>::default();

    map.add(key1, &widget1);
    map.add(key2, &widget2);

    assert_eq!(map.find(key1).as_ref(), Some(&widget1));
    assert_eq!(map.find(key2).as_ref(), Some(&widget2));

    map.remove_key(key1);

    assert_eq!(map.find(key1), None);
    assert_eq!(map.find(key2).as_ref(), Some(&widget2));

    map.remove_value(&widget2);

    assert_eq!(map.find(key1), None);
    assert_eq!(map.find(key2), None);
}
