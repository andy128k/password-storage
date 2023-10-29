use crate::cache::Cache;
use crate::primary_accel;
use crate::utils::ui::centered;
use awesome_gtk::widget::AwesomeWidgetTraverseExt;
use gtk::{glib, prelude::*};
use os_str_bytes::OsStrBytes;
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub struct PSDashboard {
    container: gtk::Widget,
    listbox: gtk::ListBox,
}

fn accel_label(accel: &str) -> Option<glib::GString> {
    let (key, mode) = gtk::accelerator_parse(accel)?;
    Some(gtk::accelerator_get_label(key, mode))
}

pub fn action_row(action: &str, label: &str, icon: &str, accel: Option<&str>) -> gtk::ListBoxRow {
    let grid = gtk::Grid::builder()
        .margin_start(10)
        .margin_end(10)
        .margin_top(10)
        .margin_bottom(10)
        .column_spacing(10)
        .build();

    let image = gtk::Image::from_icon_name(icon);
    grid.attach(&image, 0, 0, 1, 2);

    let label1 = gtk::Label::builder()
        .use_markup(true)
        .label(label)
        .margin_bottom(5)
        .halign(gtk::Align::Start)
        .hexpand(true)
        .build();
    grid.attach(&label1, 1, 0, 1, 1);

    if let Some(accel) = accel.and_then(accel_label) {
        let accel_label = gtk::Label::builder()
            .label(accel)
            .halign(gtk::Align::Start)
            .build();
        grid.attach(&accel_label, 1, 1, 2, 1);
    }

    let row = gtk::ListBoxRow::builder()
        .action_name(action)
        .css_classes(["frame"])
        .child(&grid)
        .build();

    set_group_title(&row, "Start");

    row
}

pub fn file_row(
    filename: PathBuf,
    on_remove: impl Fn(&gtk::ListBoxRow, &Path) + 'static,
) -> Option<gtk::ListBoxRow> {
    let basename = filename.file_name()?;

    let grid = gtk::Grid::builder()
        .margin_start(10)
        .margin_end(10)
        .margin_top(10)
        .margin_bottom(10)
        .build();

    let label1 = gtk::Label::builder()
        .label(basename.to_string_lossy().as_ref())
        .margin_bottom(5)
        .halign(gtk::Align::Start)
        .hexpand(true)
        .build();
    label1.add_css_class("bold");
    grid.attach(&label1, 0, 0, 1, 1);

    let remove_button = gtk::Button::builder()
        .icon_name("window-close")
        .tooltip_text("Forget this file.")
        .has_frame(false)
        .vexpand(false)
        .hexpand(false)
        .build();
    grid.attach(&remove_button, 1, 0, 1, 1);

    let label2 = gtk::Label::builder()
        .label(filename.to_string_lossy().as_ref())
        .halign(gtk::Align::Start)
        .build();
    if !filename.is_file() {
        label2.set_tooltip_text(Some("File does not exist."));
        label2.add_css_class("error");
    }
    grid.attach(&label2, 0, 1, 2, 1);

    let row = gtk::ListBoxRow::builder()
        .action_name("app.open-file")
        .action_target(&filename.to_raw_bytes().as_ref().into())
        .css_classes(["frame"])
        .child(&grid)
        .build();

    remove_button.connect_clicked(glib::clone!(@weak row => move |_| {
        on_remove(&row, &filename);
    }));

    set_group_title(&row, "Recent files");

    Some(row)
}

fn set_group_title(row: &gtk::ListBoxRow, title: &str) {
    unsafe {
        row.set_data("group_title", title.to_string());
    }
}

fn get_group_title(row: &gtk::ListBoxRow) -> Option<String> {
    unsafe {
        row.data::<String>("group_title")
            .map(|h| h.as_ref().clone())
    }
}

fn header(label: &str) -> gtk::Widget {
    let label = gtk::Label::builder()
        .use_markup(false)
        .label(label)
        .margin_start(10)
        .margin_end(10)
        .margin_top(20)
        .margin_bottom(5)
        .halign(gtk::Align::Start)
        .build();

    let grid = gtk::Grid::builder().css_classes(["background"]).build();
    grid.attach(&label, 0, 0, 1, 1);

    grid.upcast()
}

impl Default for PSDashboard {
    fn default() -> Self {
        let listbox = gtk::ListBox::builder()
            .width_request(400)
            .hexpand(true)
            .build();

        listbox.set_header_func(|row, row_before| {
            let group_before = row_before.and_then(get_group_title);
            let group = get_group_title(row).filter(|group| Some(group) != group_before.as_ref());
            if let Some(header_text) = group {
                row.set_header(Some(&header(&header_text)));
            } else {
                row.set_header(None::<&gtk::Widget>);
            }
        });

        Self {
            container: centered(&listbox),
            listbox,
        }
    }
}

impl PSDashboard {
    pub fn update(&self, cache: &Cache) {
        self.listbox.set_visible(false);
        for child in self.listbox.children() {
            if let Some(row) = child.downcast_ref::<gtk::ListBoxRow>() {
                self.listbox.remove(row);
            }
        }

        self.listbox.append(&action_row(
            "app.new",
            "New file",
            "document-new",
            Some(primary_accel!("n")),
        ));
        self.listbox.append(&action_row(
            "app.open",
            "Open...",
            "document-open",
            Some(primary_accel!("o")),
        ));

        let mut first_row = None;
        for filename in cache.recent_files() {
            if let Some(row) = file_row(
                filename,
                glib::clone!(@weak self.listbox as listbox, @strong cache => move |row, filename| {
                    cache.remove_file(filename);
                    listbox.remove(row);
                }),
            ) {
                self.listbox.append(&row);
                if first_row.is_none() {
                    first_row = Some(row);
                }
            }
        }
        self.listbox.set_visible(true);
        if let Some(row) = first_row {
            self.listbox.select_row(Some(&row));
            row.grab_focus();
        }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.container.clone().upcast()
    }
}
