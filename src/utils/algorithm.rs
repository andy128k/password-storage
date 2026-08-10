pub fn all_equal_to<T: PartialEq, I: IntoIterator<Item = T>>(iter: I) -> Option<T> {
    let mut first = None;
    for value in iter {
        match first {
            None => first = Some(value),
            Some(ref first) if *first == value => {}
            _ => return None,
        }
    }
    first
}

pub fn all_equal<T: PartialEq, I: IntoIterator<Item = T>>(iter: I) -> bool {
    all_equal_to(iter).is_some()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_all_equal_to() {
        let empty: &[i32] = &[];
        assert_eq!(all_equal_to(empty), None);
        assert_eq!(all_equal_to(&[1, 2, 1, 1]), None);
        assert_eq!(all_equal_to([3, 3, 3]), Some(3));
        assert_eq!(all_equal_to([3, 3, 3, 2, 3]), None);
    }

    #[test]
    fn test_all_equal() {
        let empty: &[i32] = &[];
        assert_eq!(all_equal(empty), false);
        assert_eq!(all_equal(&[1, 2, 1, 1]), false);
        assert_eq!(all_equal([3, 3, 3]), true);
        assert_eq!(all_equal([3, 3, 3, 2, 3]), false);
    }
}
