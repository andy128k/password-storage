use std::rc::{Rc, Weak};

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

    pub fn handler<P, R: Default>(
        &self,
        f: impl Fn(&SharedPtr<T>, P) -> Result<R, Box<dyn std::error::Error>> + 'static,
    ) -> impl Fn(P) -> R {
        let weak = self.downgrade();
        move |p| {
            weak.upgrade().map_or_else(R::default, |t| match f(&t, p) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("{:?}", e);
                    R::default()
                }
            })
        }
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
