use gtk::prelude::*;

use super::menu::create_add_entity_menu;
use crate::actions::*;
use crate::model::record::RECORD_TYPE_GENERIC;

fn button(label: &str, icon: &str, action: &PSAction) -> gtk::ToolButton {
    let image = gtk::Image::from_icon_name(Some(icon), gtk::IconSize::SmallToolbar);
    let item = gtk::ToolButton::new(Some(&image), Some(label));
    item.set_action_name(Some(action.full_name().as_ref()));
    item
}

fn button2(label: &str, icon: &str, action: &str) -> gtk::ToolButton {
    let image = gtk::Image::from_icon_name(Some(icon), gtk::IconSize::SmallToolbar);
    let item = gtk::ToolButton::new(Some(&image), Some(label));
    item.set_action_name(Some(action));
    item
}

pub fn create_tool_bar(search_entry: &gtk::Widget) -> gtk::Toolbar {
    let toolbar = gtk::Toolbar::new();

    toolbar.add(&button2("New file", "document-new", "app.new"));
    toolbar.add(&button2("Open file", "document-open", "app.open"));
    toolbar.add(&button(
        "Save file",
        "document-save",
        &PSAction::ViewMode(ViewModeAction::Save),
    ));
    toolbar.add(&gtk::SeparatorToolItem::new());
    toolbar.add(&{
        let image = gtk::Image::from_icon_name(Some("list-add"), gtk::IconSize::SmallToolbar);
        let item = gtk::MenuToolButton::new(Some(&image), Some("Add entry"));
        item.set_menu(&gtk::Menu::from_model(&create_add_entity_menu()));
        let default_action =
            PSAction::ViewMode(ViewModeAction::Add(RECORD_TYPE_GENERIC.name.to_string()));
        item.set_action_name(Some(default_action.full_name().as_ref()));
        item
    });

    toolbar.add(&gtk::SeparatorToolItem::new());

    toolbar.add(&{
        let item = gtk::ToggleToolButton::new();
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
        let item = gtk::ToolItem::new();
        ToolItemExt::set_expand(&item, true);
        item
    });

    toolbar.add(&{
        // search entry
        let item = gtk::ToolItem::new();
        item.add(search_entry);
        item
    });

    toolbar
}
