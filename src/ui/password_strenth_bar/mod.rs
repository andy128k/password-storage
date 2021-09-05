use crate::entropy::PasswordStrenth;
use crate::gtk_prelude::*;
use crate::include_css;

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
        levelbar.style_context().add_provider(
            &include_css!("style.css"),
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
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

    pub fn set_strenth(&self, strenth: Option<PasswordStrenth>) {
        let value = match strenth {
            None => 0.0,
            Some(PasswordStrenth::VeryWeak) => 1.0,
            Some(PasswordStrenth::Weak) => 2.0,
            Some(PasswordStrenth::Reasonable) => 3.0,
            Some(PasswordStrenth::Strong) => 4.0,
            Some(PasswordStrenth::VeryStrong) => 5.0,
        };
        self.levelbar.set_value(value);
    }
}
