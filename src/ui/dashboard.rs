use crate::cache::Cache;
use gtk::prelude::*;
use std::path::Path;

#[derive(Clone)]
pub struct PSDashboard {
    container: gtk::Widget,
    content: gtk::Widget,
    listbox: gtk::ListBox,
    cache: Cache,
}

fn centered<W: IsA<gtk::Widget> + WidgetExt>(widget: &W) -> gtk::Widget {
    widget.set_hexpand(true);
    widget.set_halign(gtk::Align::Center);

    let grid = gtk::Grid::new();
    grid.attach(widget, 0, 0, 1, 1);
    grid.upcast()
}

fn framed<W: IsA<gtk::Widget>>(widget: &W) -> gtk::Widget {
    let frame = gtk::Frame::new(None);
    frame.set_shadow_type(gtk::ShadowType::EtchedIn);
    frame.add(widget);
    frame.upcast()
}

mod filerow {
    use crate::markup_builder::bold;
    use gtk::prelude::*;
    use lazy_static::lazy_static;
    use std::path::PathBuf;

    lazy_static! {
        static ref FILENAME_DATA_KEY: glib::Quark = glib::Quark::from_string("filename");
    }

    pub fn create(filename: PathBuf) -> Option<gtk::ListBoxRow> {
        let basename = filename.file_name()?;

        let grid = gtk::Grid::new();
        grid.set_property_margin(10);

        let label1 = gtk::LabelBuilder::new()
            .use_markup(true)
            .label(&bold(basename.to_string_lossy().as_ref()))
            .margin_bottom(5)
            .halign(gtk::Align::Start)
            .build();
        grid.attach(&label1, 0, 0, 1, 1);

        let label2 = gtk::LabelBuilder::new()
            .label(filename.to_string_lossy().as_ref())
            .halign(gtk::Align::Start)
            .build();
        grid.attach(&label2, 0, 1, 1, 1);

        let row = gtk::ListBoxRow::new();
        row.add(&grid);

        unsafe {
            row.set_qdata(*FILENAME_DATA_KEY, filename);
        }

        Some(row)
    }

    pub fn get_filename(row: &gtk::ListBoxRow) -> Option<&PathBuf> {
        unsafe { row.get_qdata(*FILENAME_DATA_KEY) }
    }
}

impl PSDashboard {
    pub fn new(cache: &Cache) -> Self {
        let title = gtk::LabelBuilder::new()
            .label("Recent files")
            .halign(gtk::Align::Start)
            .margin_top(20)
            .margin_bottom(5)
            .build();

        let listbox = gtk::ListBoxBuilder::new().hexpand(true).build();

        let grid = gtk::GridBuilder::new().width_request(400).build();
        grid.attach(&title, 0, 0, 1, 1);
        grid.attach(&framed(&listbox), 0, 1, 1, 1);

        Self {
            container: centered(&grid),
            content: grid.upcast(),
            listbox,
            cache: cache.clone(),
        }
    }

    pub fn update(&self) {
        self.content.hide();
        for row in self.listbox.get_children() {
            self.listbox.remove(&row);
        }
        let mut first_row = None;
        let cache = &self.cache;
        for filename in cache.recent_files() {
            if let Some(row) = filerow::create(filename) {
                self.listbox.add(&row);
                if first_row.is_none() {
                    first_row = Some(row);
                }
            }
        }
        if let Some(row) = first_row {
            self.content.show_all();
            self.listbox.select_row(Some(&row));
            row.grab_focus();
        }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.container.clone().upcast()
    }

    pub fn connect_activate<F: Fn(&Path) + 'static>(&self, callback: F) {
        self.listbox
            .connect_row_activated(move |_, row| match filerow::get_filename(row) {
                Some(filename) => callback(filename),
                None => eprintln!("Cannot get filename."),
            });
    }
}
