use crate::actions::*;
use crate::model::record::RECORD_TYPES;
use crate::utils::menu_builder::*;
use gtk::{gio, glib::prelude::*};

pub fn create_add_entity_menu() -> gio::Menu {
    gio::Menu::from_items(RECORD_TYPES.iter().map(|record_type| {
        gio::MenuItem::create()
            .action(&format!("file.add-{}", record_type.name))
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
                    .action(&format!("entry.convert-to-{}", record_type.name))
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
                                .action(&PSAction::ViewMode(ViewModeAction::Save).full_name())
                                .label("_Save")
                                .accel("<Primary>s"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action(&PSAction::ViewMode(ViewModeAction::SaveAs).full_name())
                                .label("Save _As..."),
                        ),
                )
                .section(
                    gio::Menu::new().item(
                        gio::MenuItem::create()
                            .action(&PSAction::ViewMode(ViewModeAction::MergeFile).full_name())
                            .label("_Merge file"),
                    ),
                )
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action(&PSAction::ViewMode(ViewModeAction::Close).full_name())
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
                            .action(&PSAction::ViewMode(ViewModeAction::Find).full_name())
                            .label("_Find")
                            .accel("<Primary>f"),
                    ),
                )
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action(&PSAction::Record(RecordAction::CopyName).full_name())
                                .label("Copy _name")
                                .accel("<Primary>c"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action(&PSAction::Record(RecordAction::CopyPassword).full_name())
                                .label("Copy pass_word")
                                .accel("<Primary><Shift>c"),
                        ),
                )
                .section(
                    gio::Menu::new().item(
                        gio::MenuItem::create()
                            .action(&PSAction::ViewMode(ViewModeAction::ChangePassword).full_name())
                            .label("Change file _password"),
                    ),
                )
                .section(
                    gio::Menu::new()
                        .item(
                            gio::MenuItem::create()
                                .action(&PSAction::Doc(DocAction::MergeMode).full_name())
                                .label("_Merge mode"),
                        )
                        .item(
                            gio::MenuItem::create()
                                .action(
                                    &PSAction::MergeMode(MergeModeAction::UncheckAll).full_name(),
                                )
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
                .item(
                    gio::MenuItem::create()
                        .action(&PSAction::Record(RecordAction::Edit).full_name())
                        .label("_Edit"),
                )
                .submenu("_Convert", create_convert_entity_menu())
                .item(
                    gio::MenuItem::create()
                        .action(&PSAction::Record(RecordAction::Delete).full_name())
                        .label("_Delete"),
                )
                .item(
                    gio::MenuItem::create()
                        .action(&PSAction::MergeMode(MergeModeAction::Merge).full_name())
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
                        .action(&PSAction::Record(RecordAction::CopyName).full_name())
                        .label("Copy _name")
                        .accel("<Primary>c"),
                )
                .item(
                    gio::MenuItem::create()
                        .action(&PSAction::Record(RecordAction::CopyPassword).full_name())
                        .label("Copy pass_word")
                        .accel("<Primary><Shift>c"),
                ),
        )
        .section(create_add_entity_menu())
        .section(
            gio::Menu::new()
                .item(
                    gio::MenuItem::create()
                        .action(&PSAction::Record(RecordAction::Edit).full_name())
                        .label("_Edit"),
                )
                .submenu("_Convert", create_convert_entity_menu())
                .item(
                    gio::MenuItem::create()
                        .action(&PSAction::Record(RecordAction::Delete).full_name())
                        .label("_Delete"),
                ),
        )
        .upcast()
}
