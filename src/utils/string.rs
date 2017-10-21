use std::convert::AsRef;

pub fn non_empty<S: AsRef<str>>(s: S) -> Option<String> {
    if !s.as_ref().is_empty() {
        Some(s.as_ref().to_owned())
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
        assert_eq!(Some("a".to_string()), non_empty("a"));
    }
}
