use gtk::prelude::*;

use super::menu::create_add_entity_menu;
use crate::actions::*;
use crate::model::record::RECORD_TYPE_GENERIC;

fn button(label: &str, icon: &str, action: &str) -> gtk::ToolButton {
    gtk::ToolButton::builder()
        .label(label)
        .icon_name(icon)
        .action_name(action)
        .has_tooltip(true)
        .tooltip_text(label)
        .build()
}

pub fn create_tool_bar(search_entry: &gtk::Widget) -> gtk::Toolbar {
    let toolbar = gtk::Toolbar::new();

    toolbar.add(&button("New file", "document-new", "app.new"));
    toolbar.add(&button("Open file", "document-open", "app.open"));
    toolbar.add(&button(
        "Save file",
        "document-save",
        &PSAction::ViewMode(ViewModeAction::Save).full_name(),
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
        &PSAction::MergeMode(MergeModeAction::Merge).full_name(),
    ));

    toolbar.add(&{
        // expander
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
