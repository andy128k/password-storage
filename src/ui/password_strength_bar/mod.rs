use crate::entropy::PasswordStrength;
use crate::utils::style::load_static_css;
use gtk::{glib, prelude::*};

#[derive(Clone, glib::Downgrade)]
pub struct PasswordStrenthBar {
    levelbar: gtk::LevelBar,
}

impl PasswordStrenthBar {
    pub fn new() -> Self {
        let levelbar = gtk::LevelBar::builder()
            .mode(gtk::LevelBarMode::Discrete)
            .min_value(0.0)
            .max_value(5.0)
            .build();
        levelbar.style_context().add_class("password-strength-bar");
        load_static_css(&levelbar, include_str!("style.css"));
        levelbar.add_offset_value("strength-very-weak", 1.0);
        levelbar.add_offset_value("strength-weak", 2.0);
        levelbar.add_offset_value("strength-reasonable", 3.0);
        levelbar.add_offset_value("strength-strong", 4.0);
        levelbar.add_offset_value("strength-very-strong", 5.0);
        Self { levelbar }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.levelbar.clone().upcast()
    }

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
