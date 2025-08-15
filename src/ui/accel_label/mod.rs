use crate::utils::style::StaticCssExt;
use gtk::{glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::utils::ui::orphan_all_children;

    #[derive(Default)]
    pub struct AccelLabel;

    #[glib::object_subclass]
    impl ObjectSubclass for AccelLabel {
        const NAME: &'static str = "PSAccelLabel";
        type Type = super::AccelLabel;
        type ParentType = gtk::Widget;
    }

    impl ObjectImpl for AccelLabel {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();

            let layout = gtk::BoxLayout::builder()
                .orientation(gtk::Orientation::Horizontal)
                .spacing(5)
                .build();
            obj.set_layout_manager(Some(layout));
            obj.add_static_css(
                include_str!("style.css"),
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
        }
    }

    impl WidgetImpl for AccelLabel {}
}

glib::wrapper! {
    pub struct AccelLabel(ObjectSubclass<imp::AccelLabel>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl AccelLabel {
    pub fn new(accel: &[&str]) -> Self {
        let obj: Self = glib::Object::builder().build();
        for (index, btn) in accel.iter().enumerate() {
            if index > 0 {
                gtk::Label::new(Some("+")).set_parent(&obj);
            }

            let keycap = gtk::Label::new(Some(btn));
            keycap.add_css_class("ps-keycap");
            if *btn == "Shift" {
                keycap.add_css_class("ps-keycap-2_25u");
            } else if index + 1 < accel.len() {
                keycap.add_css_class("ps-keycap-1_25u");
            }
            keycap.set_parent(&obj);
        }
        obj
    }
}
