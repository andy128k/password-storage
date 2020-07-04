use gtk::prelude::*;
use gtk::{
    IconSize, Image, Menu, MenuToolButton, SeparatorToolItem, ToggleToolButton, ToolButton,
    ToolItem, Toolbar, Widget,
};

use super::menu::create_add_entity_menu;
use crate::actions::*;
use crate::model::record::RECORD_TYPE_GENERIC;

fn button(label: &str, icon: &str, action: &PSAction) -> ToolButton {
    let image = Image::from_icon_name(Some(icon), IconSize::SmallToolbar);
    let item = ToolButton::new(Some(&image), Some(label));
    item.set_action_name(Some(action.full_name().as_ref()));
    item
}

fn button2(label: &str, icon: &str, action: &str) -> ToolButton {
    let image = Image::from_icon_name(Some(icon), IconSize::SmallToolbar);
    let item = ToolButton::new(Some(&image), Some(label));
    item.set_action_name(Some(action));
    item
}

pub fn create_tool_bar(search_entry: &Widget) -> Toolbar {
    let toolbar = Toolbar::new();

    toolbar.add(&button2("New file", "document-new", "app.new"));
    toolbar.add(&button2("Open file", "document-open", "app.open"));
    toolbar.add(&button(
        "Save file",
        "document-save",
        &PSAction::ViewMode(ViewModeAction::Save),
    ));
    toolbar.add(&SeparatorToolItem::new());
    toolbar.add(&{
        let image = Image::from_icon_name(Some("list-add"), IconSize::SmallToolbar);
        let item = MenuToolButton::new(Some(&image), Some("Add entry"));
        item.set_menu(&Menu::from_model(&create_add_entity_menu()));
        let default_action =
            PSAction::ViewMode(ViewModeAction::Add(RECORD_TYPE_GENERIC.name.to_string()));
        item.set_action_name(Some(default_action.full_name().as_ref()));
        item
    });

    toolbar.add(&SeparatorToolItem::new());

    toolbar.add(&{
        let item = ToggleToolButton::new();
        item.set_label(Some("Toggle merge mode"));
        item.set_icon_name(Some("merge-mode"));
        item.set_action_name(Some(
            PSAction::Doc(DocAction::MergeMode).full_name().as_ref(),
        ));
        item
    });

    toolbar.add(&button(
        "Merge entries",
        "merge",
        &PSAction::MergeMode(MergeModeAction::Merge),
    ));

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
