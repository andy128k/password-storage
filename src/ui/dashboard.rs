use std::rc::Rc;
use std::path::PathBuf;
use gtk::prelude::*;
use gtk::{Widget, Grid, Frame, ShadowType, ListBox, ListBoxRow, Align, Label};
use markup_builder::*;
use cache::Cache;

pub struct PSDashboard {
    container: Widget,
    content: Widget,
    listbox: ListBox,
    cache: Cache,
    on_activate: Option<Rc<Fn(&PathBuf)>>
}

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

impl PSDashboard {
    pub fn new(cache: &Cache) -> Self {
        let title = Label::new("Recent files");
        title.set_halign(Align::Start);
        title.set_margin_top(20);
        title.set_margin_bottom(5);

        let listbox = ListBox::new();
        listbox.set_hexpand(true);

        let grid = Grid::new();
        grid.set_property_width_request(400);
        grid.attach(&title, 0, 0, 1, 1);
        grid.attach(&framed(&listbox), 0, 1, 1, 1);

        Self {
            container: centered(&grid),
            content: grid.upcast(),
            listbox,
            cache: cache.retain(),
            on_activate: None
        }
    }

    fn create_row(&self, filename: &PathBuf, basename: &str, path: &str) -> ListBoxRow {
        let grid = Grid::new();
        grid.set_property_margin(10);

        let label1 = Label::new(None);
        label1.set_markup(&bold(basename));
        label1.set_margin_bottom(5);
        label1.set_halign(Align::Start);
        grid.attach(&label1, 0, 0, 1, 1);

        let label2 = Label::new(path);
        label2.set_halign(Align::Start);
        grid.attach(&label2, 0, 1, 1, 1);

        let row = ListBoxRow::new();
        row.add(&grid);

        if let Some(ref callback_rc) = self.on_activate {
            let filename_copy = filename.clone();
            let callback = Rc::downgrade(callback_rc);
            row.connect_activate(move |_row| {
                if let Some(cb) = callback.upgrade() {
                    cb(&filename_copy);
                }
            });
        }

        row
    }

    pub fn update(&self) {
        self.content.hide();
        for row in self.listbox.get_children() {
            self.listbox.remove(&row);
        }
        let mut first_row = None;
        for filename in &self.cache.borrow().recent_files {
            if let Some(basename) = filename.file_name() {
                let basename = basename.to_string_lossy();
                let path = filename.to_string_lossy();

                let row = self.create_row(filename, basename.as_ref(), path.as_ref());
                self.listbox.add(&row);

                if first_row.is_none() {
                    first_row = Some(row);
                }
            }
        }
        if let Some(row) = first_row {
            self.content.show_all();
            self.listbox.select_row(&row);
            row.grab_focus();
        }
    }

    pub fn get_widget(&self) -> Widget {
        self.container.clone().upcast()
    }

    pub fn connect_activate<F: Fn(&PathBuf) + 'static>(&mut self, callback: F) {
        self.on_activate = Some(Rc::new(callback));
    }
}
