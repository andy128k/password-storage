use crate::id::Id;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type Listener<T> = Rc<dyn Fn(&T)>;

pub struct Slot<T> {
    id: Id,
    listeners: RefCell<HashMap<Id, Listener<T>>>,
}

pub struct ListenerId(Id, Id);

impl<T> Slot<T> {
    pub fn subscribe(&self, listener: impl Fn(&T) + 'static) -> ListenerId {
        let listener_id = Id::default();
        self.listeners
            .borrow_mut()
            .insert(listener_id, Rc::new(listener));
        ListenerId(self.id, listener_id)
    }

    pub fn unsubscribe(&self, listener_id: ListenerId) {
        if listener_id.0 == self.id {
            self.listeners.borrow_mut().remove(&listener_id.1);
        } else {
            eprintln!("WARN cannot unsubscribe listener");
        }
    }

    pub fn emit(&self, param: T) {
        for listener in self.listeners.borrow().values().cloned() {
            (listener)(&param);
        }
    }
}

impl<T> Default for Slot<T> {
    fn default() -> Self {
        Self {
            id: Id::default(),
            listeners: RefCell::new(HashMap::new()),
        }
    }
}
