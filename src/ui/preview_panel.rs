use crate::markup_builder::*;
use crate::model::record::Record;
use gtk::prelude::*;
use gtk::{Align, Grid, Image, Label, Widget};

pub struct PSPreviewPanel {
    grid: Grid,
    icon: Image,
    title: Label,
    view: Label,
}

impl PSPreviewPanel {
    pub fn new() -> Self {
        let grid = Grid::new();
        grid.set_property_width_request(300);
        grid.set_property_margin(16);

        let icon = Image::new();
        icon.set_margin_end(8);
        icon.set_halign(Align::Start);
        icon.set_valign(Align::Start);
        grid.attach(&icon, 0, 0, 1, 1);

        let title = Label::new(None);
        title.set_hexpand(true);
        title.set_xalign(0f32);
        title.set_line_wrap(true);
        grid.attach(&title, 1, 0, 1, 1);

        let view = Label::new(None);
        view.set_margin_top(16);
        view.set_hexpand(true);
        view.set_xalign(0f32);
        view.set_line_wrap(true);
        grid.attach(&view, 0, 2, 2, 1);

        PSPreviewPanel {
            grid,
            icon,
            title,
            view,
        }
    }

    pub fn get_widget(&self) -> Widget {
        self.grid.clone().upcast()
    }

    pub fn update(&self, record_opt: Option<Record>, show_secrets: bool) {
        if let Some(record) = record_opt {
            self.icon
                .set_property_icon_name(Some(record.record_type.icon));
            self.title.set_markup(&big(&record.name()));
            self.view
                .set_markup(&record_to_markup(&record, show_secrets));
        } else {
            self.icon.set_property_icon_name(None);
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
