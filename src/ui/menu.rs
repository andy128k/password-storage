use crate::primary_accel;
use crate::utils::menu_builder::*;
use gtk::{gio, prelude::*};

pub fn create_main_menu() -> gio::MenuModel {
    gio::Menu::new()
        .section(
            gio::Menu::new()
                .item(
                    gio::MenuItem::create()
                        .action("file.change-password")
                        .label("Change file _password"),
                )
                .item(
                    gio::MenuItem::create()
                        .action("file.merge-file")
                        .label("_Merge another file into the current one"),
                ),
        )
        .section(gio::Menu::new())
        .section(
            gio::Menu::new()
                .item(
                    gio::MenuItem::create()
                        .action("app.preferences")
                        .label("_Preferences"),
                )
                .item(
                    gio::MenuItem::create()
                        .action("app.shortcuts")
                        .label("_Keyboard shortcuts...")
                        .accel(primary_accel!("question")),
                )
                .item(
                    gio::MenuItem::create()
                        .action("app.about")
                        .label("_About..."),
                ),
        )
        .upcast()
}
