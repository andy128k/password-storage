use gtk::prelude::*;
use password_storage::utils::list_model::ListModelImmutableExt;

pub fn test_last() {
    let list = gtk::StringList::new(&["a", "b", "c", "d"]);
    let last = list.last();
    assert!(last.is_some());
    let item = last.and_downcast::<gtk::StringObject>().unwrap();
    assert_eq!(item.string(), "d");
}

pub fn test_slice() {
    let list = gtk::StringList::new(&["a", "b", "c", "d"]);
    assert_eq!(list.sliced(..).n_items(), 4);
    assert_eq!(list.sliced(1..).n_items(), 3);
    assert_eq!(list.sliced(..2).n_items(), 2);
}

pub fn test_appended() {
    let list = gtk::StringList::new(&["a", "b", "c", "d"]);
    let new_list = list.appended(&gtk::StringObject::new("e"));
    assert_eq!(new_list.n_items(), 5);
}
