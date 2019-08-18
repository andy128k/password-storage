use gtk::prelude::*;
use gtk::{Widget, Toolbar, ToolItem, SeparatorToolItem, ToolButton, MenuToolButton, Menu, ToggleToolButton, Image, IconSize};

use crate::model::record::RECORD_TYPE_GENERIC;
use crate::actions::*;
use super::menu::create_add_entity_menu;

fn button(label: &str, icon: &str, action: &PSAction) -> ToolButton {
    let image = Image::new_from_icon_name(Some(icon), IconSize::SmallToolbar);
    let item = ToolButton::new(Some(&image), Some(label));
    item.set_action_name(Some(action.full_name().as_ref()));
    item
}

pub fn create_tool_bar(search_entry: &Widget) -> Toolbar {
    let toolbar = Toolbar::new();

    toolbar.add(&button("New file", "document-new", &PSAction::App(AppAction::New)));
    toolbar.add(&button("Open file", "document-open", &PSAction::App(AppAction::Open)));
    toolbar.add(&button("Save file", "document-save", &PSAction::ViewMode(ViewModeAction::Save)));
    toolbar.add(&SeparatorToolItem::new());
    toolbar.add(&{
        let image = Image::new_from_icon_name(Some("list-add"), IconSize::SmallToolbar);
        let item = MenuToolButton::new(Some(&image), Some("Add entry"));
        item.set_menu(&Menu::new_from_model(&create_add_entity_menu()));
        let default_action = PSAction::ViewMode(ViewModeAction::Add(RECORD_TYPE_GENERIC.name.to_string()));
        item.set_action_name(Some(default_action.full_name().as_ref()));
        item
    });

    toolbar.add(&SeparatorToolItem::new());

    toolbar.add(&{
        let item = ToggleToolButton::new();
        item.set_label(Some("Toggle merge mode"));
        item.set_icon_name(Some("merge-mode"));
        item.set_action_name(Some(PSAction::Doc(DocAction::MergeMode).full_name().as_ref()));
        item
    });

    toolbar.add(&button("Merge entries", "merge", &PSAction::MergeMode(MergeModeAction::Merge)));

    toolbar.add(&{
        // expander / separator
        let item = ToolItem::new();
        item.set_expand(true);
        item
    });

    toolbar.add(&{
        // search entry
        let item = ToolItem::new();
        item.add(search_entry);
        item
    });

    toolbar
}
