use super::menu::create_add_entity_menu;
use crate::actions::*;
use crate::utils::toolbar_builder::*;

pub fn create_tool_bar(search_entry: &gtk::Widget) -> gtk::Widget {
    ToolbarBuilder::new()
        .button("New file", "document-new", "app.new")
        .button("Open file", "document-open", "app.open")
        .button(
            "Save file",
            "document-save",
            &PSAction::ViewMode(ViewModeAction::Save).full_name(),
        )
        .separator()
        .menu_button("Add entry", "list-add", &create_add_entity_menu())
        .separator()
        .toggle_button(
            "Toggle merge mode",
            "merge-mode",
            PSAction::Doc(DocAction::MergeMode).full_name().as_ref(),
        )
        .button(
            "Merge entries",
            "merge",
            &PSAction::MergeMode(MergeModeAction::Merge).full_name(),
        )
        .expander()
        .add(search_entry)
        .build()
}
