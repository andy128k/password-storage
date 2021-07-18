use std::convert::AsRef;

pub trait StringExt: Sized {
    fn non_empty(self) -> Option<Self>;
}

impl<T: AsRef<str>> StringExt for T {
    fn non_empty(self) -> Option<Self> {
        if !self.as_ref().is_empty() {
            Some(self)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_non_empty() {
        assert_eq!(None, "".non_empty());
        assert_eq!(Some("a"), "a".non_empty());
    }
}
