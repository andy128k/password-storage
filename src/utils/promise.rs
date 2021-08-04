use futures::channel::oneshot::{channel, Receiver, Sender};
use std::cell::RefCell;

pub struct Promise<T>(RefCell<Option<Sender<T>>>);

impl<T> Promise<T> {
    pub fn new() -> (Promise<T>, Receiver<T>) {
        let (sender, receiver) = channel::<T>();
        let promise = Promise::from_sender(sender);
        (promise, receiver)
    }

    fn from_sender(sender: Sender<T>) -> Self {
        Self(RefCell::new(Some(sender)))
    }

    pub fn fulfill(&self, value: T) {
        if let Some(sender) = self.0.take() {
            if sender.send(value).is_err() {
                eprintln!("WARNING: no receiver");
            }
        }
    }
}
