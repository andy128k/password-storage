use crate::gtk_prelude::*;

pub fn create_merge_bar() -> gtk::InfoBar {
    let merge_mode_bar = gtk::InfoBar::new();
    merge_mode_bar.content_area().pack_start(
        &gtk::Label::builder().label("Merge mode").build(),
        false,
        false,
        0,
    );
    merge_mode_bar.action_area().unwrap().pack_end(
        &gtk::Button::builder()
            .label("Merge selected entries")
            .action_name("merge.merge")
            .build(),
        false,
        false,
        0,
    );
    merge_mode_bar.action_area().unwrap().pack_end(
        &gtk::Button::builder()
            .label("Deselect all")
            .action_name("merge.uncheck-all")
            .build(),
        false,
        false,
        0,
    );
    merge_mode_bar.action_area().unwrap().pack_end(
        &gtk::Button::builder()
            .label("Back to normal mode")
            .action_name("doc.merge-mode")
            .build(),
        false,
        false,
        0,
    );

    merge_mode_bar
}
