use std::rc::{Rc, Weak};
use glib::clone::{Downgrade, Upgrade};

pub struct SharedPtr<T>(Rc<debug_cell::RefCell<T>>);

pub struct WeakPtr<T>(Weak<debug_cell::RefCell<T>>);

impl<T> SharedPtr<T> {
    pub fn from_private(value: T) -> Self {
        SharedPtr::<T>(Rc::new(debug_cell::RefCell::new(value)))
    }

    pub fn retain(&self) -> Self {
        SharedPtr::<T>(self.0.clone())
    }

    pub fn downgrade(&self) -> WeakPtr<T> {
        WeakPtr::<T>(Rc::downgrade(&self.0))
    }

    pub fn borrow(&self) -> debug_cell::Ref<'_, T> {
        let cell: &debug_cell::RefCell<T> = &self.0;
        cell.borrow()
    }

    pub fn borrow_mut(&self) -> debug_cell::RefMut<'_, T> {
        let cell: &debug_cell::RefCell<T> = &self.0;
        cell.borrow_mut()
    }
}

impl<T> WeakPtr<T> {
    pub fn upgrade(&self) -> Option<SharedPtr<T>> {
        self.0.upgrade().map(SharedPtr::<T>)
    }
}

impl<T> Downgrade for SharedPtr<T> {
    type Weak = WeakPtr<T>;

    fn downgrade(&self) -> Self::Weak {
        SharedPtr::downgrade(self)
    }
}

impl<T> Upgrade for WeakPtr<T> {
    type Strong = SharedPtr<T>;

    fn upgrade(&self) -> Option<Self::Strong> {
        WeakPtr::upgrade(self)
    }
}
