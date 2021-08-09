use crate::cache::Cache;
use crate::gtk_prelude::*;
use crate::markup_builder::bold;
use os_str_bytes::OsStrBytes;
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub struct PSDashboard {
    container: gtk::Widget,
    listbox: gtk::ListBox,
}

fn centered<W: IsA<gtk::Widget> + WidgetExt>(widget: &W) -> gtk::Widget {
    widget.set_hexpand(true);
    widget.set_halign(gtk::Align::Center);

    let grid = gtk::Grid::new();
    grid.attach(widget, 0, 0, 1, 1);
    grid.upcast()
}

thread_local! {
    static CSS_PROVIDER: gtk::CssProvider = {
        let provider = gtk::CssProvider::new();
        provider
            .load_from_data(br##"
                label.error {
                    color: #FF3333;
                }
            "##);
        provider
    };
}

fn accel_label(accel: &str) -> Option<glib::GString> {
    let (key, mode) = gtk::accelerator_parse(accel)?;
    gtk::accelerator_get_label(key, mode)
}

pub fn action_row(action: &str, label: &str, icon: &str, accel: Option<&str>) -> gtk::ListBoxRow {
    let grid = gtk::Grid::builder()
        .margin_start(10)
        .margin_end(10)
        .margin_top(10)
        .margin_bottom(10)
        .column_spacing(10)
        .build();

    let image = gtk::Image::from_icon_name(Some(icon));
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
            .label(&accel)
            .halign(gtk::Align::Start)
            .build();
        grid.attach(&accel_label, 1, 1, 2, 1);
    }

    let row = gtk::ListBoxRow::builder().action_name(action).build();
    row.set_child(Some(&grid));

    let context = row.style_context();
    context.add_provider(
        &CSS_PROVIDER.with(Clone::clone),
        gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
    );
    context.add_class("frame");

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
        .use_markup(true)
        .label(&bold(basename.to_string_lossy().as_ref()))
        .margin_bottom(5)
        .halign(gtk::Align::Start)
        .hexpand(true)
        .build();
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
        let context = label2.style_context();
        context.add_provider(
            &CSS_PROVIDER.with(Clone::clone),
            gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
        );
        context.add_class("error");
    }
    grid.attach(&label2, 0, 1, 2, 1);

    let row = gtk::ListBoxRow::builder().build();
    row.set_child(Some(&grid));

    row.set_action_name(Some("app.open-file"));
    row.set_action_target_value(Some(&glib::Variant::from_bytes::<Vec<u8>>(
        &glib::Bytes::from(filename.to_raw_bytes().as_ref()),
    )));

    remove_button.connect_clicked(clone!(@weak row => move |_| {
        on_remove(&row, &filename);
    }));

    let context = row.style_context();
    context.add_provider(
        &CSS_PROVIDER.with(Clone::clone),
        gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
    );
    context.add_class("frame");

    Some(row)
}

fn set_header(row: &gtk::ListBoxRow, header: &str) {
    unsafe {
        row.set_data("header", header.to_string());
    }
}

fn get_header(row: &gtk::ListBoxRow) -> Option<String> {
    unsafe { row.data::<String>("header").map(|h| h.as_ref().clone()) }
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

    let grid = gtk::Grid::new();
    let context = grid.style_context();
    context.add_class("background");

    grid.attach(&label, 0, 0, 1, 1);

    grid.show();
    grid.upcast()
}

impl PSDashboard {
    pub fn new() -> Self {
        let listbox = gtk::ListBox::builder()
            .width_request(400)
            .hexpand(true)
            .build();

        listbox.set_header_func(|row, _row_before| {
            if let Some(header_text) = get_header(row) {
                row.set_header(Some(&header(&header_text)));
            }
        });

        Self {
            container: centered(&listbox),
            listbox,
        }
    }

    pub fn update(&self, cache: &Cache) {
        self.listbox.hide();
        self.remove_all();

        let new = action_row("app.new", "New file", "document-new", Some("<Primary>n"));
        set_header(&new, "Start");
        self.listbox.append(&new);
        self.listbox.append(&action_row(
            "app.open",
            "Open...",
            "document-open",
            Some("<Primary>o"),
        ));

        let mut first_row = None;
        for filename in cache.recent_files() {
            if let Some(row) = file_row(
                filename,
                clone!(@weak self.listbox as listbox, @strong cache => move |row, filename| {
                    cache.remove_file(filename);
                    listbox.remove(row);
                }),
            ) {
                self.listbox.append(&row);
                if first_row.is_none() {
                    set_header(&row, "Recent files");
                    first_row = Some(row);
                }
            }
        }
        self.listbox.show();
        if let Some(row) = first_row {
            self.listbox.show();
            self.listbox.select_row(Some(&row));
            row.grab_focus();
        }
    }

    fn remove_all(&self) {
        let mut child_opt = self.listbox.first_child();
        while let Some(child) = child_opt {
            let next = child.next_sibling();
            self.listbox.remove(&child);
            child_opt = next;
        }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.container.clone().upcast()
    }
}
