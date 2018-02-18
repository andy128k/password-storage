use std::rc::{Rc, Weak};

pub struct SharedPtr<T>(Rc<::debug_cell::RefCell<T>>);

pub struct WeakPtr<T>(Weak<::debug_cell::RefCell<T>>);

impl<T> SharedPtr<T> {
    pub fn from_private(value: T) -> Self {
        SharedPtr::<T>(Rc::new(::debug_cell::RefCell::new(value)))
    }

    pub fn retain(&self) -> Self {
        SharedPtr::<T>(self.0.clone())
    }

    pub fn downgrade(&self) -> WeakPtr<T> {
        WeakPtr::<T>(Rc::downgrade(&self.0))
    }

    pub fn borrow(&self) -> ::debug_cell::Ref<T> {
        let cell: &::debug_cell::RefCell<T> = &self.0;
        cell.borrow()
    }

    pub fn borrow_mut(&self) -> ::debug_cell::RefMut<T> {
        let cell: &::debug_cell::RefCell<T> = &self.0;
        cell.borrow_mut()
    }
}

impl<T> WeakPtr<T> {
    pub fn new() -> Self {
        WeakPtr::<T>(Weak::new())
    }

    pub fn upgrade(&self) -> Option<SharedPtr<T>> {
        self.0.upgrade().map(SharedPtr::<T>)
    }
}
