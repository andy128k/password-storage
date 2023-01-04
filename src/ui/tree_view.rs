use crate::gtk_prelude::*;
use crate::model::record::Record;
use crate::store::TreeStoreColumn;
use gdk::ffi::{GDK_BUTTON_PRIMARY, GDK_BUTTON_SECONDARY};

mod imp {
    use super::*;
    use crate::utils::ui::{scrolled, PSWidgetExt};

    #[derive(Default)]
    pub struct PSTreeView {
        pub view: gtk::TreeView,
        pub column: gtk::TreeViewColumn,
        pub url_column: gtk::TreeViewColumn,
        pub url_icon_renderer: gtk::CellRendererPixbuf,
        pub description_column: gtk::TreeViewColumn,
        pub url_click: gtk::GestureClick,
        pub popup_click: gtk::GestureClick,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSTreeView {
        const NAME: &'static str = "PSTreeView";
        type Type = super::PSTreeView;
        type ParentType = gtk::Widget;
    }

    impl ObjectImpl for PSTreeView {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(&gtk::BinLayout::new()));

            self.view.set_can_focus(true);
            self.view.set_headers_visible(true);
            // FIXME: reordering in GTK4 does not allow to control drop destination (e.g. move into group only)
            // self.view.set_reorderable(true);
            self.view.set_search_column(1);
            self.view.set_grid_lines(gtk::TreeViewGridLines::Horizontal);
            self.view.selection().set_mode(gtk::SelectionMode::Multiple);
            scrolled(&self.view).set_parent(&*obj);

            self.column.set_title("Name");
            self.column.set_sizing(gtk::TreeViewColumnSizing::Autosize);
            self.column.set_expand(true);
            self.column.set_spacing(5);

            let icon = gtk::CellRendererPixbuf::new();
            icon.set_padding(0, 4);
            icon.set_icon_size(gtk::IconSize::Large);
            self.column.pack_start(&icon, false);
            self.column
                .add_attribute(&icon, "icon-name", TreeStoreColumn::TypeIcon.into());

            let text = gtk::CellRendererText::new();
            self.column.pack_start(&text, true);
            self.column
                .add_attribute(&text, "text", TreeStoreColumn::Name.into());

            self.view.append_column(&self.column);

            let description_renderer = gtk::CellRendererText::new();
            self.description_column.set_title("Description");
            self.description_column.set_expand(true);
            self.description_column
                .pack_start(&description_renderer, true);
            self.description_column.add_attribute(
                &description_renderer,
                "text",
                TreeStoreColumn::Description.into(),
            );
            self.view.append_column(&self.description_column);

            self.view.append_column(&{
                let column = gtk::TreeViewColumn::new();
                column.set_title("Strength");
                column.set_sizing(gtk::TreeViewColumnSizing::Fixed);

                let strength = gtk::CellRendererPixbuf::new();
                strength.set_icon_size(gtk::IconSize::Normal);
                strength.set_padding(16, 0);
                column.pack_start(&strength, false);
                column.add_attribute(&strength, "icon-name", TreeStoreColumn::Strength.into());

                column
            });

            self.url_icon_renderer.set_icon_size(gtk::IconSize::Normal);

            self.url_column.set_title("Actions");
            self.url_column.set_sizing(gtk::TreeViewColumnSizing::Fixed);
            self.url_column
                .pack_start(&gtk::CellRendererText::new(), true);
            self.url_column.pack_start(&self.url_icon_renderer, false);
            self.url_column
                .pack_start(&gtk::CellRendererText::new(), true);
            self.url_column.add_attribute(
                &self.url_icon_renderer,
                "icon-name",
                TreeStoreColumn::ShareIcon.into(),
            );

            self.view.append_column(&self.url_column);

            self.url_click.set_button(GDK_BUTTON_PRIMARY as u32);
            self.view.add_controller(&self.url_click);

            self.popup_click.set_button(GDK_BUTTON_SECONDARY as u32);
            self.view.add_controller(&self.popup_click);
        }

        fn dispose(&self) {
            for child in self.obj().children() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PSTreeView {}
}

glib::wrapper! {
    pub struct PSTreeView(ObjectSubclass<imp::PSTreeView>)
        @extends gtk::Widget;
}

impl Default for PSTreeView {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

fn get_real_iter(
    view: &PSTreeView,
    path: &gtk::TreePath,
) -> Option<(gtk::TreeModel, gtk::TreeIter)> {
    let model = view.imp().view.model()?;
    let iter = model.iter(path)?;
    Some((model, iter))
}

fn view_selection(view: &gtk::TreeView) -> Option<gtk::TreeSelection> {
    // Workaround. See https://github.com/gtk-rs/gtk4-rs/issues/584
    unsafe {
        use gtk::glib::translate::ToGlibPtr;
        if gtk::ffi::gtk_tree_view_get_selection(
            view.upcast_ref::<gtk::TreeView>().to_glib_none().0,
        )
        .is_null()
        {
            return None;
        }
    }
    Some(view.selection())
}

fn get_selected_iter(view: &gtk::TreeView) -> Option<(Record, gtk::TreeIter, gtk::TreePath)> {
    let (mut paths, model) = view_selection(view)?.selected_rows();
    if paths.len() != 1 {
        return None;
    }
    let path = paths.pop()?;
    let iter = model.iter(&path)?;
    let record = model.get::<Record>(&iter, TreeStoreColumn::Record.into());
    Some((record, iter, path))
}

fn select_iter(view: &gtk::TreeView, iter: &gtk::TreeIter) {
    let Some(current_model) = view.model() else { return };
    let path = current_model.path(iter);

    view.expand_to_path(&path);
    view.scroll_to_cell(Some(&path), None::<&gtk::TreeViewColumn>, true, 0.5, 0.0);
    gtk::prelude::TreeViewExt::set_cursor(view, &path, None::<&gtk::TreeViewColumn>, false);
    view.selection().select_path(&path);
}

impl PSTreeView {
    pub fn set_model(&self, model: Option<&gtk::TreeModel>) {
        self.imp().view.set_model(model);
    }

    fn record_of_clicked_url(&self, event_x: f64, event_y: f64) -> Option<Record> {
        let (x, y) = self
            .imp()
            .view
            .convert_widget_to_bin_window_coords(event_x as i32, event_y as i32);
        let (path, col, cell_x, _cell_y) = self.imp().view.path_at_pos(x, y)?;
        let path = path?;
        let col = col?;
        if col != self.imp().url_column {
            return None;
        }

        let (cell_pos, cell_width) = self
            .imp()
            .url_column
            .cell_get_position(&self.imp().url_icon_renderer)?;
        if cell_pos <= cell_x && cell_x <= cell_pos + cell_width {
            let (model, iter) = get_real_iter(self, &path)?;
            let record = model.get::<Record>(&iter, TreeStoreColumn::Record.into());
            return Some(record);
        }
        None
    }

    pub fn connect_url_clicked<F: Fn(Record) + 'static>(&self, handler: F) {
        self.imp().url_click.connect_pressed(
            clone!(@weak self as this => move |_gesture, _n, x, y| {
                if let Some(record) = this.record_of_clicked_url(x, y) {
                    handler(record);
                }
            }),
        );
    }

    pub fn get_selected_record(&self) -> Option<(Record, gtk::TreeIter)> {
        let (record, iter, _path) = get_selected_iter(&self.imp().view)?;
        Some((record, iter))
    }

    pub fn get_selected_records(&self) -> (Vec<Record>, Vec<gtk::TreeIter>) {
        let Some(selection) = view_selection(&self.imp().view) else { return (vec![], vec![]) };
        let (paths, model) = selection.selected_rows();
        let iters: Vec<_> = paths.iter().filter_map(|p| model.iter(p)).collect();
        let records = iters
            .iter()
            .map(|iter| model.get::<Record>(iter, TreeStoreColumn::Record.into()))
            .collect();
        (records, iters)
    }

    pub fn connect_record_changed<F: Fn(&[Record]) + 'static>(&self, changed: F) {
        self.imp().view.selection().connect_changed(
            clone!(@weak self as this => move |_selection| {
                let (records, _) = this.get_selected_records();
                changed(&records);
            }),
        );
    }

    pub fn connect_record_activated<F: Fn(Record, gtk::TreeIter) + 'static>(&self, activated: F) {
        let view = &self.imp().view;
        view.connect_row_activated(move |view, _iter, _col| {
            let Some((record, iter, path)) = get_selected_iter(view) else { return };
            if record.record_type.is_group {
                // toggle group
                if view.row_expanded(&path) {
                    view.collapse_row(&path);
                } else {
                    view.expand_row(&path, false);
                }
            } else {
                activated(record, iter);
            }
        });
    }

    pub fn set_popup(&self, popup_model: &gio::MenuModel) {
        let context_menu = gtk::PopoverMenu::builder()
            .autohide(true)
            .menu_model(popup_model)
            .build();
        context_menu.set_parent(self);

        let view = self.imp().view.clone();
        self.imp().popup_click.connect_pressed(
            clone!(@weak view, @strong context_menu => move |_gesture, _n, x, y| {
                view.grab_focus();
                if let Some((Some(path), _, _, _)) = view.path_at_pos(x as i32, y as i32) {
                    gtk::prelude::TreeViewExt::set_cursor(&view, &path, None::<&gtk::TreeViewColumn>, false);

                    let row_rect = view.cell_area(Some(&path), None);

                    context_menu.set_pointing_to(Some(&gdk::Rectangle::new(
                        x as i32,
                        row_rect.y(),
                        0,
                        row_rect.height(),
                    )));
                } else {
                    context_menu.set_pointing_to(Some(&gdk::Rectangle::new(
                        x as i32,
                        y as i32,
                        0,
                        0,
                    )));
                }
                context_menu.popup();
            }),
        );
    }

    pub fn toggle_group(&self, path: &gtk::TreePath) {
        if self.imp().view.row_expanded(path) {
            self.imp().view.collapse_row(path);
        } else {
            self.imp().view.expand_row(path, false);
        }
    }

    pub fn select_iter(&self, iter: &gtk::TreeIter) {
        select_iter(&self.imp().view, iter);
    }
}
