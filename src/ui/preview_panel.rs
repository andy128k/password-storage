use crate::markup_builder::*;
use crate::model::record::Record;
use gtk::{glib, prelude::*};

pub struct PSPreviewPanel {
    grid: gtk::Grid,
    icon: gtk::Image,
    title: gtk::Label,
    view: gtk::Label,
}

impl PSPreviewPanel {
    pub fn new() -> Self {
        let grid = gtk::Grid::builder()
            .width_request(300)
            .margin_start(16)
            .margin_end(16)
            .margin_top(16)
            .margin_bottom(16)
            .build();

        let icon = gtk::Image::builder()
            .margin_end(8)
            .halign(gtk::Align::Start)
            .valign(gtk::Align::Start)
            .build();
        grid.attach(&icon, 0, 0, 1, 1);

        let title = gtk::Label::builder()
            .hexpand(true)
            .xalign(0_f32)
            .wrap(true)
            .build();
        grid.attach(&title, 1, 0, 1, 1);

        let view = gtk::Label::builder()
            .margin_top(16)
            .hexpand(true)
            .xalign(0_f32)
            .wrap(true)
            .build();
        grid.attach(&view, 0, 2, 2, 1);

        Self {
            grid,
            icon,
            title,
            view,
        }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.grid.clone().upcast()
    }

    pub fn update(&self, record_opt: Option<Record>, show_secrets: bool) {
        if let Some(record) = record_opt {
            self.icon.set_icon_name(Some(record.record_type.icon));
            self.title.set_markup(&big(&record.name()));
            self.view
                .set_markup(&record_to_markup(&record, show_secrets));
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
                        buf.push_str(&glib::markup_escape_text(&value));
                    }
                }
                "url" => {
                    if !buf.is_empty() {
                        buf.push('\n');
                    }
                    buf.push_str(&bold(field.title));
                    buf.push(' ');
                    buf.push_str(&url(&value));
                }
                _ => {
                    if !buf.is_empty() {
                        buf.push('\n');
                    }
                    buf.push_str(&bold(field.title));
                    buf.push(' ');
                    buf.push_str(&glib::markup_escape_text(&value));
                }
            }
        }
    }
    buf
}
