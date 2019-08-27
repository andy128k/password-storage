use glib::prelude::*;
use gio::{MenuModel, Menu, MenuItem, Icon};
use crate::actions::*;
use crate::model::record::RECORD_TYPES;

fn item(label: &str, action: &PSAction, accel: Option<&str>, icon: Option<&str>) -> MenuItem {
    let item = MenuItem::new(Some(label), Some(action.full_name().as_ref()));

    if let Some(accel) = accel {
        item.set_attribute_value("accel", Some(&accel.into()));
    }

    if let Some(icon_name) = icon {
        match Icon::new_for_string(icon_name) {
            Ok(icon) => item.set_icon(&icon),
            Err(e) => println!("Cannot find icon {}. Reason: {}", icon_name, e)
        }
    }

    item
}

trait MenuBuilderExt {
    fn item(self, label: &str, action: &PSAction) -> Self;
    fn item_with_accel(self, label: &str, action: &PSAction, accel: &str) -> Self;
    fn submenu(self, label: &str, submenu: MenuModel) -> Self;
}

impl MenuBuilderExt for Menu {
    fn item(self, label: &str, action: &PSAction) -> Self {
        self.append_item(&item(label, action, None, None));
        self
    }

    fn item_with_accel(self, label: &str, action: &PSAction, accel: &str) -> Self {
        self.append_item(&item(label, action, Some(accel), None));
        self
    }

    fn submenu(self, label: &str, submenu: MenuModel) -> Self {
        self.append_submenu(Some(label), &submenu);
        self
    }
}

pub fn create_add_entity_menu() -> MenuModel {
    let menu = Menu::new();
    for record_type in RECORD_TYPES.iter() {
        let label = format!("Add {}", record_type.name);
        let action = PSAction::ViewMode(ViewModeAction::Add(record_type.name.to_string()));
        let item = item(&label, &action, None, Some(record_type.icon));
        menu.append_item(&item);
    }
    menu.upcast()
}

pub fn create_convert_entity_menu() -> MenuModel {
    let menu = Menu::new();
    for record_type in RECORD_TYPES.iter() {
        if !record_type.is_group {
            let label = format!("Convert to {}", record_type.name);
            let action = PSAction::Record(RecordAction::ConvertTo(record_type.name.to_string()));
            let item = item(&label, &action, None, Some(record_type.icon));
            menu.append_item(&item);
        }
    }
    menu.upcast()
}

pub fn create_menu_bar() -> MenuModel {
    let menu = Menu::new();
    menu.append_submenu(Some("_File"), &{
        let menu = Menu::new();
        menu.append_section(None, &{
            Menu::new()
                .item_with_accel("_New", &PSAction::App(AppAction::New), "<Primary>n")
                .item_with_accel("_Open", &PSAction::App(AppAction::Open), "<Primary>o")
                .item_with_accel("_Save", &PSAction::ViewMode(ViewModeAction::Save), "<Primary>s")
                .item("Save _As...", &PSAction::ViewMode(ViewModeAction::SaveAs))
        });
        menu.append_section(None, &{
            Menu::new()
                .item("_Merge file", &PSAction::ViewMode(ViewModeAction::MergeFile))
        });
        menu.append_section(None, &{
            Menu::new()
                .item_with_accel("_Close", &PSAction::ViewMode(ViewModeAction::Close), "<Primary>w")
                .item_with_accel("_Quit", &PSAction::App(AppAction::Quit), "<Primary>q")
        });
        menu
    });
    menu.append_submenu(Some("_Edit"), &{
        let menu = Menu::new();
        menu.append_section(None, &{
            Menu::new()
                .item_with_accel("_Find", &PSAction::ViewMode(ViewModeAction::Find), "<Primary>f")
        });
        menu.append_section(None, &{
            Menu::new()
                .item_with_accel("Copy _name", &PSAction::Record(RecordAction::CopyName), "<Primary>c")
                .item_with_accel("Copy pass_word", &PSAction::Record(RecordAction::CopyPassword), "<Primary><Shift>c")
        });
        menu.append_section(None, &{
            Menu::new()
                .item("Change file _password", &PSAction::ViewMode(ViewModeAction::ChangePassword))
        });
        menu.append_section(None, &{
            Menu::new()
                .item("_Merge mode", &PSAction::Doc(DocAction::MergeMode))
                .item("Uncheck all", &PSAction::MergeMode(MergeModeAction::UncheckAll))
        });
        menu.append_section(None, &{
            Menu::new()
                .item("_Preferences", &PSAction::App(AppAction::Preferences))
        });
        menu
    });
    menu.append_submenu(Some("_Entry"), &{
        Menu::new()
            .submenu("_Add", create_add_entity_menu())
            .item("_Edit", &PSAction::Record(RecordAction::Edit))
            .submenu("_Convert", create_convert_entity_menu())
            .item("_Delete", &PSAction::Record(RecordAction::Delete))
            .item("_Merge", &PSAction::MergeMode(MergeModeAction::Merge))
    });
    menu.append_submenu(Some("_Help"), &{
        Menu::new()
            .item("_About...", &PSAction::App(AppAction::About))
    });
    menu.upcast()
}

pub fn create_tree_popup() -> MenuModel {
    let menu = Menu::new();
    menu.append_section(None, &{
        Menu::new()
            .item_with_accel("Copy _name", &PSAction::Record(RecordAction::CopyName), "<Primary>c")
            .item_with_accel("Copy pass_word", &PSAction::Record(RecordAction::CopyPassword), "<Primary><Shift>c")
    });
    menu.append_section(None, &create_add_entity_menu());
    menu.append_section(None, &{
        Menu::new()
            .item("_Edit", &PSAction::Record(RecordAction::Edit))
            .submenu("_Convert", create_convert_entity_menu())
            .item("_Delete", &PSAction::Record(RecordAction::Delete))
    });
    menu.upcast()
}
