pub fn all_equal_by_key<'a, T, K: PartialEq>(
    values: &'a [T],
    key_fn: impl Fn(&'a T) -> K + Copy + 'a,
) -> Option<&T> {
    let (first, rest) = values.split_first()?;
    let first_key = (key_fn)(first);
    if rest.iter().all(|value| (key_fn)(value) == first_key) {
        Some(first)
    } else {
        None
    }
}

pub fn all_equal<T: PartialEq>(values: &[T]) -> Option<&T> {
    all_equal_by_key(values, |x| x)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_all_equal_by_key() {
        assert_eq!(all_equal_by_key::<i32, i32>(&[], |x| x % 2), None);
        assert_eq!(all_equal_by_key(&[1, 2, 1, 1], |x| x % 2), None);
        assert_eq!(all_equal_by_key(&[3, 3, 3], |x| x % 2), Some(&3));
        assert_eq!(all_equal_by_key(&[3, 5, 7], |x| x % 2), Some(&3));
    }

    #[test]
    fn test_all_equal() {
        assert_eq!(all_equal::<i32>(&[]), None);
        assert_eq!(all_equal(&[1, 2, 1, 1]), None);
        assert_eq!(all_equal(&[3, 3, 3]), Some(&3));
    }
}
