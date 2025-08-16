use crate::model::record::RECORD_TYPES;
use crate::ui::record_type_popover::RecordTypePopoverBuilder;
use crate::utils::ui::{action_button, action_popover_button};
use gtk::prelude::*;

pub fn create_action_bar() -> gtk::Widget {
    let action_bar = gtk::ActionBar::builder().hexpand(true).build();
    action_bar.pack_start(&action_popover_button(
        &RecordTypePopoverBuilder::default()
            .record_types(RECORD_TYPES)
            .action_name_func(|record_type| format!("file.add::{}", record_type.name))
            .build(),
        "ps-add",
        "Add new record",
    ));
    action_bar.pack_start(&action_button("file.delete", "ps-remove", "Remove record"));
    action_bar.pack_start(&action_button("file.edit", "ps-edit", "Edit record"));
    action_bar.pack_start(&action_button(
        "file.merge",
        "merge",
        "Merge selected records",
    ));
    action_bar.pack_end(&action_button(
        "file.copy-password",
        "dialog-password-symbolic",
        "Copy password",
    ));
    action_bar.pack_end(&action_button(
        "file.copy-name",
        "edit-copy-symbolic",
        "Copy name",
    ));
    action_bar.upcast()
}
