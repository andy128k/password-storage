use std::path::{Path, PathBuf};
use gtk::prelude::*;
use gtk::{Widget, Grid, Frame, ShadowType, ListBox, ListBoxRow, Align, Label};
use crate::markup_builder::bold;
use crate::cache::Cache;
use crate::utils::object_data::{object_get_data, object_set_data};
use crate::ptr::*;

pub struct PSDashboardPrivate {
    container: Widget,
    content: Widget,
    listbox: ListBox,
    cache: Cache,
    on_activate: Option<Box<dyn Fn(&Path)>>
}

pub type PSDashboard = SharedPtr<PSDashboardPrivate>;

fn centered<W: IsA<Widget> + WidgetExt>(widget: &W) -> Widget {
    widget.set_hexpand(true);
    widget.set_halign(Align::Center);

    let grid = Grid::new();
    grid.attach(widget, 0, 0, 1, 1);
    grid.upcast()
}

fn framed<W: IsA<Widget>>(widget: &W) -> Widget {
    let frame = Frame::new(None);
    frame.set_shadow_type(ShadowType::EtchedIn);
    frame.add(widget);
    frame.upcast()
}

const FILENAME_DATA_KEY: &str = "filename";

fn create_row(filename: &PathBuf, basename: &str, path: &str) -> ListBoxRow {
    let grid = Grid::new();
    grid.set_property_margin(10);

    let label1 = Label::new(None);
    label1.set_markup(&bold(basename));
    label1.set_margin_bottom(5);
    label1.set_halign(Align::Start);
    grid.attach(&label1, 0, 0, 1, 1);

    let label2 = Label::new(Some(path));
    label2.set_halign(Align::Start);
    grid.attach(&label2, 0, 1, 1, 1);

    let row = ListBoxRow::new();
    row.add(&grid);

    object_set_data(&row, FILENAME_DATA_KEY, filename).unwrap();

    row
}

impl PSDashboard {
    pub fn new(cache: &Cache) -> PSDashboard {
        let title = Label::new(Some("Recent files"));
        title.set_halign(Align::Start);
        title.set_margin_top(20);
        title.set_margin_bottom(5);

        let listbox = ListBox::new();
        listbox.set_hexpand(true);

        let grid = Grid::new();
        grid.set_property_width_request(400);
        grid.attach(&title, 0, 0, 1, 1);
        grid.attach(&framed(&listbox), 0, 1, 1, 1);

        let dashboard = PSDashboard::from_private(PSDashboardPrivate {
            container: centered(&grid),
            content: grid.upcast(),
            listbox,
            cache: cache.retain(),
            on_activate: None
        });

        {
            let this = dashboard.downgrade();
            dashboard.borrow().listbox.connect_row_activated(move |_, row| {
                if let Some(dashboard) = this.upgrade() {
                    if let Some(ref cb) = dashboard.borrow().on_activate {
                        let filename: PathBuf = object_get_data(row, FILENAME_DATA_KEY).unwrap();
                        cb(&filename);
                    }
                }
            });
        }

        dashboard
    }

    pub fn update(&self) {
        self.borrow().content.hide();
        for row in self.borrow().listbox.get_children() {
            self.borrow().listbox.remove(&row);
        }
        let mut first_row = None;
        let cache = &self.borrow().cache;
        for filename in &cache.borrow().recent_files {
            if let Some(basename) = filename.file_name() {
                let basename = basename.to_string_lossy();
                let path = filename.to_string_lossy();

                let row = create_row(filename, basename.as_ref(), path.as_ref());
                self.borrow().listbox.add(&row);

                if first_row.is_none() {
                    first_row = Some(row);
                }
            }
        }
        if let Some(row) = first_row {
            self.borrow().content.show_all();
            self.borrow().listbox.select_row(Some(&row));
            row.grab_focus();
        }
    }

    pub fn get_widget(&self) -> Widget {
        self.borrow().container.clone().upcast()
    }

    pub fn connect_activate<F: Fn(&Path) + 'static>(&mut self, callback: F) {
        self.borrow_mut().on_activate = Some(Box::new(callback));
    }
}
