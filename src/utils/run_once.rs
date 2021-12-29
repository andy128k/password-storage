use std::collections::BTreeSet;
use std::sync::Mutex;

#[derive(Default)]
pub struct RunOnce<K>(Mutex<BTreeSet<K>>);

impl<K: Ord> RunOnce<K> {
    pub fn run(&self, key: K, run: impl Fn()) {
        let mut guard = self.0.lock().unwrap();
        if !guard.contains(&key) {
            run();
            guard.insert(key);
        }
    }
}
