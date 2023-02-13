use password_storage::utils::ui::{bitset_iter, bitset_iter_rev};

pub fn test_bitset_iter() {
    let empty = gtk::Bitset::new_empty();
    let range = gtk::Bitset::new_range(12, 3);
    let odds = gtk::Bitset::new_empty();
    odds.add(3);
    odds.add(7);
    odds.add(5);

    assert_eq!(bitset_iter(&empty).count(), 0);
    assert_eq!(bitset_iter(&range).collect::<Vec<u32>>(), vec![12, 13, 14]);
    assert_eq!(bitset_iter(&odds).collect::<Vec<u32>>(), vec![3, 5, 7]);

    assert_eq!(bitset_iter_rev(&empty).count(), 0);
    assert_eq!(
        bitset_iter_rev(&range).collect::<Vec<u32>>(),
        vec![14, 13, 12]
    );
    assert_eq!(bitset_iter_rev(&odds).collect::<Vec<u32>>(), vec![7, 5, 3]);
}
