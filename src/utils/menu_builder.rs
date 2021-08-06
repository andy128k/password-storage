use crate::gtk_prelude::*;

pub trait MenuItemBuilderExt {
    fn create() -> Self;
    fn action(self, action: &str) -> Self;
    fn label(self, label: &str) -> Self;
    fn accel(self, accel: &str) -> Self;
    fn icon(self, icon: &str) -> Self;
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

    fn icon(self, icon: &str) -> Self {
        match gio::Icon::for_string(icon) {
            Ok(icon) => self.set_icon(&icon),
            Err(e) => eprintln!("Cannot find icon {}. Reason: {}", icon, e),
        }
        self
    }
}

pub trait MenuBuilderExt {
    fn from_items(items: impl Iterator<Item = gio::MenuItem>) -> Self;
    fn item(self, item: gio::MenuItem) -> Self;
    fn submenu(self, label: &str, submenu: gio::Menu) -> Self;
    fn section(self, section: gio::Menu) -> Self;
}

impl MenuBuilderExt for gio::Menu {
    fn from_items(items: impl Iterator<Item = gio::MenuItem>) -> Self {
        let menu = gio::Menu::new();
        for item in items {
            menu.append_item(&item);
        }
        menu
    }

    fn item(self, item: gio::MenuItem) -> Self {
        self.append_item(&item);
        self
    }

    fn submenu(self, label: &str, submenu: gio::Menu) -> Self {
        self.append_submenu(Some(label), &submenu);
        self
    }

    fn section(self, section: gio::Menu) -> Self {
        self.append_section(None, &section);
        self
    }
}
