use super::menu::create_add_entity_menu;
use crate::gtk_prelude::*;
use crate::utils::toolbar_builder::*;

pub fn create_tool_bar() -> gtk::Widget {
    ToolbarBuilder::new()
        .button("New file", "document-new", "app.new")
        .button("Open file", "document-open", "app.open")
        .button("Save file", "document-save", "file.save")
        .separator()
        .menu_button("Add entry", "list-add", &create_add_entity_menu())
        .separator()
        .toggle_button("Toggle merge mode", "merge-mode", "doc.merge-mode")
        .button("Merge entries", "merge", "merge.merge")
        .build()
}
