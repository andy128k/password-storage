use std::sync::atomic::{AtomicI32, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(i32);

impl std::default::Default for Id {
    fn default() -> Self {
        static LAST_ID: AtomicI32 = AtomicI32::new(1);
        Self(LAST_ID.fetch_add(1, Ordering::SeqCst))
    }
}
