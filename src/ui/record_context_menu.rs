use crate::primary_accel;
use crate::utils::menu_builder::*;
use gtk::{gio, prelude::*};

const ACTION_COPY_NAME: &str = "copy-name";
const ACTION_COPY_PASSWORD: &str = "copy-password";
const ACTION_MOVE: &str = "move";
const ACTION_EDIT: &str = "edit";
const ACTION_DELETE: &str = "delete";

pub fn record_context_menu() -> gio::MenuModel {
    gio::Menu::new()
        .section(
            gio::Menu::new()
                .item(
                    gio::MenuItem::create()
                        .action(ACTION_COPY_NAME)
                        .label("Copy _name")
                        .accel(primary_accel!("c")),
                )
                .item(
                    gio::MenuItem::create()
                        .action(ACTION_COPY_PASSWORD)
                        .label("Copy pass_word")
                        .accel(primary_accel!("<Shift>c")),
                ),
        )
        .section(
            gio::Menu::new().item(
                gio::MenuItem::create()
                    .action(ACTION_MOVE)
                    .label("_Move to..."),
            ),
        )
        .section(
            gio::Menu::new()
                .item(gio::MenuItem::create().action(ACTION_EDIT).label("_Edit"))
                .item(
                    gio::MenuItem::create()
                        .action(ACTION_DELETE)
                        .label("_Delete"),
                ),
        )
        .upcast()
}
