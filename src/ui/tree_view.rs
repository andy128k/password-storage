use crate::gtk_prelude::*;
use crate::store::TreeStoreColumn;
use gdk::ffi::{GDK_BUTTON_PRIMARY, GDK_BUTTON_SECONDARY};

mod imp {
    use super::*;
    use crate::utils::ui::{scrolled, PSWidgetExt};

    #[derive(Default)]
    pub struct PSTreeView {
        pub view: gtk::TreeView,
        pub column: gtk::TreeViewColumn,
        pub check_renderer: gtk::CellRendererToggle,
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
            scrolled(&self.view).set_parent(&*obj);

            self.column.set_title("Name");
            self.column.set_sizing(gtk::TreeViewColumnSizing::Autosize);
            self.column.set_expand(true);
            self.column.set_spacing(5);

            self.check_renderer.set_visible(false);
            self.check_renderer.connect_toggled(
                clone!(@weak self as imp => move |_renderer, path| {
                    imp.selection_toggled(&path)
                }),
            );
            self.column.pack_start(&self.check_renderer, false);

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

    impl PSTreeView {
        fn selection_toggled(&self, path: &gtk::TreePath) {
            let view = self.obj();
            if let Some((model, iter)) = get_real_iter(&*view, path) {
                if let Ok(store) = model.downcast::<gtk::TreeStore>() {
                    let selected = store.get::<bool>(&iter, TreeStoreColumn::Selection.into());
                    store.set_value(
                        &iter,
                        TreeStoreColumn::Selection.into(),
                        &glib::Value::from(&!selected),
                    );
                }
            }
        }
    }
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

fn get_selected_iter(view: &gtk::TreeView) -> Option<(gtk::TreeIter, gtk::TreePath)> {
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

    let (current_model, iter) = view.selection().selected()?;
    let path = current_model.path(&iter);
    Some((iter, path))
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

    fn iter_of_clicked_url(&self, event_x: f64, event_y: f64) -> Option<gtk::TreeIter> {
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
            let (_model, iter) = get_real_iter(self, &path)?;
            return Some(iter);
        }
        None
    }

    pub fn connect_url_clicked<F: Fn(gtk::TreeIter) + 'static>(&self, handler: F) {
        self.imp().url_click.connect_pressed(
            clone!(@weak self as this => move |_gesture, _n, x, y| {
                if let Some(iter) = this.iter_of_clicked_url(x, y) {
                    handler(iter);
                }
            }),
        );
    }

    pub fn connect_drop<F: Fn(gtk::TreeIter) -> bool + 'static>(&self, is_group: F) {
        // self.view
        //     .connect_drag_motion(move |view, drag_context, x, y, time| {
        //         if let Some((Some(path), pos)) = view.dest_row_at_pos(x, y) {
        //             if pos == gtk::TreeViewDropPosition::IntoOrBefore
        //                 || pos == gtk::TreeViewDropPosition::IntoOrAfter
        //             {
        //                 if let Some((_model, iter)) = get_real_iter(view, &path) {
        //                     if !is_group(iter) {
        //                         drag_context.drag_status(gdk::DragAction::empty(), time); // deny
        //                         return true; // stop propagation
        //                     }
        //                 }
        //             }
        //         }
        //         drag_context.drag_status(gdk::DragAction::MOVE, time);
        //         false
        //     });
    }

    pub fn connect_cursor_changed_iter<F: Fn(Option<(gtk::TreeIter, gtk::TreePath)>) + 'static>(
        &self,
        changed: F,
    ) {
        let view = &self.imp().view;
        view.connect_cursor_changed(move |view| {
            changed(get_selected_iter(view));
        });
    }

    pub fn connect_row_activated_iter<F: Fn(Option<(gtk::TreeIter, gtk::TreePath)>) + 'static>(
        &self,
        activated: F,
    ) {
        let view = &self.imp().view;
        view.connect_row_activated(move |view, _iter, _col| {
            activated(get_selected_iter(view));
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

    pub fn get_selected_iter(&self) -> Option<(gtk::TreeIter, gtk::TreePath)> {
        get_selected_iter(&self.imp().view)
    }

    pub fn select_iter(&self, iter: &gtk::TreeIter) {
        select_iter(&self.imp().view, iter);
    }

    pub fn set_selection_mode(&self, selection: bool) {
        if selection {
            self.imp().column.add_attribute(
                &self.imp().check_renderer,
                "visible",
                TreeStoreColumn::SelectionVisible.into(),
            );
            self.imp().column.add_attribute(
                &self.imp().check_renderer,
                "active",
                TreeStoreColumn::Selection.into(),
            );
        } else {
            self.imp()
                .column
                .clear_attributes(&self.imp().check_renderer);
            self.imp().check_renderer.set_visible(false);
        }
        self.imp().column.queue_resize();
    }
}
