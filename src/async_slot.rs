use crate::id::Id;
use std::{cell::RefCell, collections::HashMap, future::Future, pin::Pin, rc::Rc};

pub type AsyncListener<T, R> = dyn Fn(&T) -> Pin<Box<dyn Future<Output = R>>> + 'static;

pub struct AsyncSlot<T, R = ()> {
    id: Id,
    listeners: RefCell<HashMap<Id, Rc<AsyncListener<T, R>>>>,
}

pub struct AsyncListenerId(Id, Id);

impl<T, R> AsyncSlot<T, R> {
    pub fn subscribe(
        &self,
        listener: impl Fn(&T) -> Pin<Box<dyn Future<Output = R>>> + 'static,
    ) -> AsyncListenerId {
        let listener_id = Id::default();
        self.listeners
            .borrow_mut()
            .insert(listener_id, Rc::new(listener));
        AsyncListenerId(self.id, listener_id)
    }

    pub fn unsubscribe(&self, listener_id: AsyncListenerId) {
        if listener_id.0 == self.id {
            self.listeners.borrow_mut().remove(&listener_id.1);
        } else {
            eprintln!("WARN cannot unsubscribe listener");
        }
    }

    pub async fn emit(&self, param: T) -> Option<R> {
        let listeners = self
            .listeners
            .borrow()
            .values()
            .cloned()
            .collect::<Vec<_>>();

        let mut result = None;
        for listener in listeners {
            let value = (listener)(&param).await;
            result.replace(value);
        }
        result
    }
}

impl<T, R> Default for AsyncSlot<T, R> {
    fn default() -> Self {
        Self {
            id: Id::default(),
            listeners: RefCell::new(HashMap::new()),
        }
    }
}
