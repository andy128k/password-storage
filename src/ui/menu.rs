use crate::gtk_prelude::*;
use crate::model::record::RECORD_TYPES;
use crate::utils::menu_builder::*;

pub fn create_add_entity_menu() -> gio::Menu {
    gio::Menu::from_items(RECORD_TYPES.iter().map(|record_type| {
        gio::MenuItem::create()
            .action(&format!("file.add::{}", record_type.name))
            .label(&format!("Add {}", record_type.name))
            .icon(record_type.icon)
    }))
}

pub fn create_convert_entity_menu() -> gio::Menu {
    gio::Menu::from_items(
        RECORD_TYPES
            .iter()
            .filter(|rt| !rt.is_group)
            .map(|record_type| {
                gio::MenuItem::create()
                    .action(&format!("entry.convert-to::{}", record_type.name))
                    .label(&format!("Convert to {}", record_type.name))
                    .icon(record_type.icon)
            }),
    )
}

pub fn create_menu_bar() -> gio::MenuModel {
    gio::Menu::new()
        .submenu("_File", {
            gio::Menu::new()
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action("app.new")
                                .label("_New")
                                .accel("<Primary>n"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action("app.open")
                                .label("_Open")
                                .accel("<Primary>o"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action("file.save")
                                .label("_Save")
                                .accel("<Primary>s"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action("file.save-as")
                                .label("Save _As..."),
                        ),
                )
                .section(
                    gio::Menu::new().item(
                        gio::MenuItem::create()
                            .action("file.merge-file")
                            .label("_Merge file"),
                    ),
                )
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action("file.close")
                                .label("_Close")
                                .accel("<Primary>w"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action("app.quit")
                                .label("_Quit")
                                .accel("<Primary>q"),
                        ),
                )
        })
        .submenu("_Edit", {
            gio::Menu::new()
                .section(
                    gio::Menu::new().item(
                        gio::MenuItem::create()
                            .action("file.find")
                            .label("_Find")
                            .accel("<Primary>f"),
                    ),
                )
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action("entry.copy-name")
                                .label("Copy _name")
                                .accel("<Primary>c"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action("entry.copy-password")
                                .label("Copy pass_word")
                                .accel("<Primary><Shift>c"),
                        ),
                )
                .section(
                    gio::Menu::new().item(
                        gio::MenuItem::create()
                            .action("file.change-password")
                            .label("Change file _password"),
                    ),
                )
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action("doc.merge-mode")
                                .label("_Merge mode"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action("merge.uncheck-all")
                                .label("Uncheck all"),
                        ),
                )
                .section(
                    gio::Menu::new().item(
                        gio::MenuItem::create()
                            .action("app.preferences")
                            .label("_Preferences"),
                    ),
                )
        })
        .submenu("_Entry", {
            gio::Menu::new()
                .submenu("_Add", create_add_entity_menu())
                .item(gio::MenuItem::create().action("entry.edit").label("_Edit"))
                .submenu("_Convert", create_convert_entity_menu())
                .item(
                    gio::MenuItem::create()
                        .action("entry.delete")
                        .label("_Delete"),
                )
                .item(
                    gio::MenuItem::create()
                        .action("merge.merge")
                        .label("_Merge"),
                )
        })
        .submenu("_Help", {
            gio::Menu::new().item(
                gio::MenuItem::create()
                    .action("app.about")
                    .label("_About..."),
            )
        })
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
                        .accel("<Primary>c"),
                )
                .item(
                    gio::MenuItem::create()
                        .action("entry.copy-passwors")
                        .label("Copy pass_word")
                        .accel("<Primary><Shift>c"),
                ),
        )
        .section(create_add_entity_menu())
        .section(
            gio::Menu::new()
                .item(gio::MenuItem::create().action("entry.edit").label("_Edit"))
                .submenu("_Convert", create_convert_entity_menu())
                .item(
                    gio::MenuItem::create()
                        .action("entry.delete")
                        .label("_Delete"),
                ),
        )
        .upcast()
}
