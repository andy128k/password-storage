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
                )
                .item(
                    gio::MenuItem::create()
                        .action("doc.merge-mode")
                        .label("_Merge mode"),
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

pub fn create_tree_popup() -> gio::MenuModel {
    gio::Menu::new()
        .section(
            gio::Menu::new()
                .item(
                    gio::MenuItem::create()
                        .action("entry.copy-name")
                        .label("Copy _name")
                        .accel(primary_accel!("c")),
                )
                .item(
                    gio::MenuItem::create()
                        .action("entry.copy-password")
                        .label("Copy pass_word")
                        .accel(primary_accel!("<Shift>c")),
                ),
        )
        .section(
            gio::Menu::new().item(
                gio::MenuItem::create()
                    .action("entry.move")
                    .label("_Move to..."),
            ),
        )
        .section(
            gio::Menu::new()
                .item(gio::MenuItem::create().action("entry.edit").label("_Edit"))
                .item(
                    gio::MenuItem::create()
                        .action("entry.delete")
                        .label("_Delete"),
                ),
        )
        .upcast()
}
