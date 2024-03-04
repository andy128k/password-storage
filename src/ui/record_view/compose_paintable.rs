use gtk::{gdk, glib, graphene, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use std::cell::OnceCell;

    #[derive(Default)]
    pub struct PSBackgroundPaintable {
        pub next: OnceCell<gdk::Paintable>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSBackgroundPaintable {
        const NAME: &'static str = "PSBackgroundPaintable";
        type Type = super::PSBackgroundPaintable;
        type ParentType = glib::Object;
        type Interfaces = (gdk::Paintable,);
    }

    impl ObjectImpl for PSBackgroundPaintable {}

    impl PaintableImpl for PSBackgroundPaintable {
        fn intrinsic_width(&self) -> i32 {
            match self.next.get() {
                Some(next) => next.intrinsic_width(),
                None => self.parent_intrinsic_width(),
            }
        }

        fn intrinsic_height(&self) -> i32 {
            match self.next.get() {
                Some(next) => next.intrinsic_height(),
                None => self.parent_intrinsic_height(),
            }
        }

        fn snapshot(&self, snapshot: &gdk::Snapshot, width: f64, height: f64) {
            snapshot.append_color(
                &gdk::RGBA::new(0.5, 0.5, 0.8, 1.0),
                &graphene::Rect::new(0.0, 0.0, width as f32, height as f32),
            );
            if let Some(next) = self.next.get() {
                next.snapshot(snapshot, width, height);
            }
        }
    }
}

glib::wrapper! {
    pub struct PSBackgroundPaintable(ObjectSubclass<imp::PSBackgroundPaintable>)
        @implements gdk::Paintable;
}

impl PSBackgroundPaintable {
    pub fn new(next: impl IsA<gdk::Paintable>) -> Self {
        let obj: Self = glib::Object::builder().build();
        obj.imp().next.set(next.upcast()).unwrap();
        obj
    }
}
