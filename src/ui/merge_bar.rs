use crate::gtk_prelude::*;
use crate::utils::also::Also;

pub fn create_merge_bar() -> gtk::InfoBar {
    let merge_mode_bar = gtk::InfoBar::new();
    merge_mode_bar
        .content_area()
        .add(
            &gtk::Label::builder()
                .label("Merge mode")
                .build()
                .also(|label| {
                    label.style_context().add_class("heading");
                }),
        );
    merge_mode_bar.add_action_widget(
        &gtk::Button::builder()
            .label("Merge selected entries")
            .action_name("merge.merge")
            .build(),
        gtk::ResponseType::None,
    );
    merge_mode_bar.add_action_widget(
        &gtk::Button::builder()
            .label("Deselect all")
            .action_name("merge.uncheck-all")
            .build(),
        gtk::ResponseType::None,
    );
    merge_mode_bar.add_action_widget(
        &gtk::Button::builder()
            .label("Back to normal mode")
            .action_name("doc.merge-mode")
            .build(),
        gtk::ResponseType::None,
    );
    merge_mode_bar
}
