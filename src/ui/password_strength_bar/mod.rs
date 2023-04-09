use crate::entropy::PasswordStrength;
use crate::utils::style::StaticCssExt;
use gtk::{glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;

    #[derive(Default)]
    pub struct PasswordStrengthBar {
        levelbar: gtk::LevelBar,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PasswordStrengthBar {
        const NAME: &'static str = "PSPasswordStrengthBar";
        type Type = super::PasswordStrengthBar;
        type ParentType = gtk::Widget;
    }

    impl ObjectImpl for PasswordStrengthBar {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));
            self.levelbar.set_parent(&*obj);

            self.levelbar.set_mode(gtk::LevelBarMode::Discrete);
            self.levelbar.set_min_value(0.0);
            self.levelbar.set_max_value(5.0);
            self.levelbar.add_offset_value("strength-very-weak", 1.0);
            self.levelbar.add_offset_value("strength-weak", 2.0);
            self.levelbar.add_offset_value("strength-reasonable", 3.0);
            self.levelbar.add_offset_value("strength-strong", 4.0);
            self.levelbar.add_offset_value("strength-very-strong", 5.0);

            self.levelbar.set_height_request(8);
            self.levelbar.add_css_class("password-strength-bar");
            self.levelbar.add_static_css(
                include_str!("style.css"),
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
        }

        fn dispose(&self) {
            while let Some(child) = self.obj().first_child() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PasswordStrengthBar {}

    impl PasswordStrengthBar {
        pub fn set_strength(&self, strength: Option<PasswordStrength>) {
            let value = match strength {
                None => 0.0,
                Some(PasswordStrength::VeryWeak) => 1.0,
                Some(PasswordStrength::Weak) => 2.0,
                Some(PasswordStrength::Reasonable) => 3.0,
                Some(PasswordStrength::Strong) => 4.0,
                Some(PasswordStrength::VeryStrong) => 5.0,
            };
            self.levelbar.set_value(value);
            self.levelbar
                .set_tooltip_text(strength.map(PasswordStrength::display));
        }
    }
}

glib::wrapper! {
    pub struct PasswordStrengthBar(ObjectSubclass<imp::PasswordStrengthBar>)
        @extends gtk::Widget;
}

impl Default for PasswordStrengthBar {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl PasswordStrengthBar {
    pub fn set_strength(&self, strength: Option<PasswordStrength>) {
        self.imp().set_strength(strength);
    }
}
