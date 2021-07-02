use std::convert::AsRef;

pub fn non_empty<S: AsRef<str>>(s: S) -> Option<S> {
    if !s.as_ref().is_empty() {
        Some(s)
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_non_empty() {
        assert_eq!(None, non_empty(""));
        assert_eq!(Some("a"), non_empty("a"));
    }
}
