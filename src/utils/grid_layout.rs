use gtk::{glib, prelude::*};

pub trait PSGridLayoutExt {
    fn grid_attach(&self, child: &impl glib::IsA<gtk::Widget>) -> gtk::GridLayoutChild;
}

impl<T: glib::IsA<gtk::Widget>> PSGridLayoutExt for T {
    fn grid_attach(&self, child: &impl glib::IsA<gtk::Widget>) -> gtk::GridLayoutChild {
        child.set_parent(self);
        self.layout_manager()
            .and_downcast::<gtk::GridLayout>()
            .expect("GridLayout is expected")
            .layout_child(child)
            .downcast::<gtk::GridLayoutChild>()
            .expect("GridLayoutChild is expected")
    }
}
