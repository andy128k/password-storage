use crate::gtk_prelude::*;
use crate::markup_builder::*;
use crate::model::record::Record;
use crate::ui::menu::create_convert_entity_menu;
use crate::utils::ui::{action_button, action_menu_button};

pub struct PSPreviewPanel {
    grid: gtk::Grid,
    icon: gtk::Image,
    title: gtk::Label,
    view: gtk::Label,
    convert_button: gtk::MenuButton,
}

impl PSPreviewPanel {
    pub fn new() -> Self {
        let grid = gtk::Grid::builder().width_request(300).build();

        let icon = gtk::Image::builder()
            .margin_start(16)
            .margin_end(8)
            .margin_top(16)
            .halign(gtk::Align::Start)
            .valign(gtk::Align::Start)
            .build();
        grid.attach(&icon, 0, 0, 1, 1);

        let title = gtk::Label::builder()
            .margin_top(16)
            .hexpand(true)
            .xalign(0_f32)
            .wrap(true)
            .build();
        grid.attach(&title, 1, 0, 1, 1);

        let view = gtk::Label::builder()
            .margin_start(16)
            .margin_end(16)
            .margin_top(16)
            .margin_bottom(16)
            .hexpand(true)
            .vexpand(true)
            .valign(gtk::Align::Start)
            .xalign(0_f32)
            .wrap(true)
            .build();
        grid.attach(&view, 0, 2, 2, 1);

        let action_bar = gtk::ActionBar::builder().hexpand(true).build();
        action_bar.pack_start(&action_button("entry.edit", "text-editor-symbolic", "Edit"));
        let convert_button = action_menu_button(
            &create_convert_entity_menu(),
            "system-run-symbolic",
            "Convert to another type",
        );
        action_bar.pack_start(&convert_button);
        action_bar.pack_start(&action_button(
            "entry.delete",
            "edit-delete-symbolic",
            "Delete",
        ));
        action_bar.pack_end(&action_button(
            "entry.copy-password",
            "dialog-password-symbolic",
            "Copy password",
        ));
        action_bar.pack_end(&action_button(
            "entry.copy-name",
            "edit-copy-symbolic",
            "Copy name",
        ));
        grid.attach(&action_bar, 0, 3, 2, 1);

        Self {
            grid,
            icon,
            title,
            view,
            convert_button,
        }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.grid.clone().upcast()
    }

    pub fn update(&self, record_opt: Option<Record>, show_secrets: bool) {
        if let Some(record) = record_opt {
            self.icon
                .set_from_icon_name(Some(record.record_type.icon), gtk::IconSize::Dialog);
            self.title.set_markup(&big(&record.name()));
            self.view
                .set_markup(&record_to_markup(&record, show_secrets));
            self.convert_button
                .set_sensitive(!record.record_type.is_group);
        } else {
            self.icon.set_icon_name(None);
            self.title.set_markup("");
            self.view.set_markup("");
        }
    }
}

fn record_to_markup(record: &Record, show_secrets: bool) -> String {
    let mut buf = String::new();
    for field in &record.record_type.fields {
        if show_secrets || !field.field_type.is_secret() {
            let value = record.get_field(field);
            match field.name {
                "name" => {}
                "description" => {
                    if !value.is_empty() {
                        if !buf.is_empty() {
                            buf.push('\n');
                            buf.push('\n');
                        }
                        buf.push_str(&glib::markup_escape_text(value));
                    }
                }
                "url" => {
                    if !buf.is_empty() {
                        buf.push('\n');
                    }
                    buf.push_str(&bold(field.title));
                    buf.push(' ');
                    buf.push_str(&url(value));
                }
                _ => {
                    if !buf.is_empty() {
                        buf.push('\n');
                    }
                    buf.push_str(&bold(field.title));
                    buf.push(' ');
                    buf.push_str(&glib::markup_escape_text(value));
                }
            }
        }
    }
    buf
}
