use crate::gtk_prelude::*;

pub trait MenuItemBuilderExt {
    fn create() -> Self;
    fn action(self, action: &str) -> Self;
    fn label(self, label: &str) -> Self;
    fn accel(self, accel: &str) -> Self;
}

impl MenuItemBuilderExt for gio::MenuItem {
    fn create() -> Self {
        gio::MenuItem::new(None, None)
    }

    fn action(self, action: &str) -> Self {
        self.set_detailed_action(action);
        self
    }

    fn label(self, label: &str) -> Self {
        self.set_label(Some(label));
        self
    }

    fn accel(self, accel: &str) -> Self {
        self.set_attribute_value("accel", Some(&accel.to_variant()));
        self
    }
}

pub trait MenuBuilderExt {
    fn item(self, item: gio::MenuItem) -> Self;
    fn section(self, section: gio::Menu) -> Self;
}

impl MenuBuilderExt for gio::Menu {
    fn item(self, item: gio::MenuItem) -> Self {
        self.append_item(&item);
        self
    }

    fn section(self, section: gio::Menu) -> Self {
        self.append_section(None, &section);
        self
    }
}
