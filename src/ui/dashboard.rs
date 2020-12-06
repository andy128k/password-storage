use crate::cache::Cache;
use glib::clone;
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
    use glib::clone;
    use gtk::prelude::*;
    use std::path::{Path, PathBuf};

    thread_local! {
        static FILENAME_DATA_KEY: glib::Quark = glib::Quark::from_string("filename");

        static CSS_PROVIDER: gtk::CssProvider = {
            let provider = gtk::CssProvider::new();
            provider
                .load_from_data(br##"label.error { color: #FF3333; }"##)
                .expect("CSS is loaded");
            provider
        };
    }

    pub fn create(
        filename: PathBuf,
        on_remove: impl Fn(&gtk::ListBoxRow, &Path) + 'static,
    ) -> Option<gtk::ListBoxRow> {
        let basename = filename.file_name()?;

        let grid = gtk::Grid::new();
        grid.set_property_margin(10);

        let label1 = gtk::LabelBuilder::new()
            .use_markup(true)
            .label(&bold(basename.to_string_lossy().as_ref()))
            .margin_bottom(5)
            .halign(gtk::Align::Start)
            .hexpand(true)
            .build();
        grid.attach(&label1, 0, 0, 1, 1);

        let remove_button =
            gtk::Button::from_icon_name(Some("window-close"), gtk::IconSize::SmallToolbar);
        remove_button.set_tooltip_text(Some("Forget this file."));
        remove_button.set_relief(gtk::ReliefStyle::None);
        remove_button.set_vexpand(false);
        remove_button.set_hexpand(false);
        grid.attach(&remove_button, 1, 0, 1, 1);

        let label2 = gtk::LabelBuilder::new()
            .label(filename.to_string_lossy().as_ref())
            .halign(gtk::Align::Start)
            .build();
        if !filename.is_file() {
            label2.set_tooltip_text(Some("File does not exist."));
            let context = label2.get_style_context();
            context.add_provider(
                &CSS_PROVIDER.with(Clone::clone),
                gtk::STYLE_PROVIDER_PRIORITY_FALLBACK,
            );
            context.add_class(&gtk::STYLE_CLASS_ERROR);
        }
        grid.attach(&label2, 0, 1, 2, 1);

        let row = gtk::ListBoxRow::new();
        row.add(&grid);

        unsafe {
            row.set_qdata(FILENAME_DATA_KEY.with(Clone::clone), filename);
        }

        remove_button.connect_clicked(clone!(@weak row => move |_| {
            if let Some(filename) = get_filename(&row) {
                on_remove(&row, filename);
            }
        }));

        Some(row)
    }

    pub fn get_filename(row: &gtk::ListBoxRow) -> Option<&PathBuf> {
        unsafe { row.get_qdata(FILENAME_DATA_KEY.with(Clone::clone)) }
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
            if let Some(row) = filerow::create(
                filename,
                clone!(@weak self.listbox as listbox, @strong cache => move |row, filename| {
                    cache.remove_file(filename);
                    listbox.remove(row);
                }),
            ) {
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
