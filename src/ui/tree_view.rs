use crate::gtk_prelude::*;
use crate::store::TreeStoreColumn;
use guard::guard;

const GDK_BUTTON_SECONDARY: u32 = 3;

#[derive(Clone, glib::Downgrade)]
pub struct PSTreeView {
    pub view: gtk::TreeView,
    column: gtk::TreeViewColumn,
    check_renderer: gtk::CellRendererToggle,
    url_column: gtk::TreeViewColumn,
    description_column: gtk::TreeViewColumn,
}

fn get_real_iter(
    view: &gtk::TreeView,
    path: &gtk::TreePath,
) -> Option<(gtk::TreeModel, gtk::TreeIter)> {
    let model = view.model()?;
    let iter = model.iter(path)?;
    Some((model, iter))
}

pub fn get_selected_iter(view: &gtk::TreeView) -> Option<(gtk::TreeIter, gtk::TreePath)> {
    let (current_model, iter) = view.selection().selected()?;
    let path = current_model.path(&iter)?;
    Some((iter, path))
}

pub fn select_iter(view: &gtk::TreeView, iter: &gtk::TreeIter) {
    guard!(let Some(current_model) = view.model() else { return });
    guard!(let Some(path) = current_model.path(iter) else { return });

    view.expand_to_path(&path);
    view.scroll_to_cell(Some(&path), None::<&gtk::TreeViewColumn>, true, 0.5, 0.0);
    view.set_cursor(&path, None::<&gtk::TreeViewColumn>, false);
    view.selection().select_path(&path);
}

impl PSTreeView {
    pub fn new() -> Self {
        let view = gtk::TreeView::new();
        view.set_can_focus(true);
        view.set_headers_visible(true);
        view.set_reorderable(true);
        view.set_search_column(1);

        let column = gtk::TreeViewColumn::new();
        column.set_title("Name");
        column.set_sizing(gtk::TreeViewColumnSizing::Autosize);
        column.set_expand(true);
        column.set_spacing(5);

        let check_renderer = gtk::CellRendererToggle::new();
        check_renderer.set_visible(false);
        check_renderer.connect_toggled(clone!(@weak view => move |_renderer, path| {
            selection_toggled(&view, &path)
        }));
        column.pack_start(&check_renderer, false);

        let icon = gtk::CellRendererPixbuf::new();
        icon.set_property_stock_size(gtk::IconSize::LargeToolbar);
        column.pack_start(&icon, false);
        column.add_attribute(&icon, "icon-name", TreeStoreColumn::TypeIcon.into());

        let text = gtk::CellRendererText::new();
        column.pack_start(&text, true);
        column.add_attribute(&text, "text", TreeStoreColumn::Name.into());

        view.append_column(&column);

        let description_column = gtk::TreeViewColumn::new();
        let description_renderer = gtk::CellRendererText::new();
        description_column.set_title("Description");
        description_column.pack_start(&description_renderer, true);
        description_column.add_attribute(
            &description_renderer,
            "text",
            TreeStoreColumn::Description.into(),
        );
        view.append_column(&description_column);

        view.append_column(&{
            let column = gtk::TreeViewColumn::new();
            column.set_title("Strength");
            column.set_sizing(gtk::TreeViewColumnSizing::Fixed);

            let strength = gtk::CellRendererPixbuf::new();
            strength.set_property_stock_size(gtk::IconSize::Menu);
            strength.set_padding(16, 0);
            column.pack_start(&strength, false);
            column.add_attribute(&strength, "icon-name", TreeStoreColumn::Strength.into());

            column
        });

        let url_icon = gtk::CellRendererPixbuf::new();
        url_icon.set_property_stock_size(gtk::IconSize::Menu);

        let url_column = gtk::TreeViewColumn::new();
        url_column.set_title("Actions");
        url_column.set_sizing(gtk::TreeViewColumnSizing::Fixed);
        url_column.pack_start(&gtk::CellRendererText::new(), true);
        url_column.pack_start(&url_icon, false);
        url_column.pack_start(&gtk::CellRendererText::new(), true);
        url_column.add_attribute(&url_icon, "icon-name", TreeStoreColumn::ShareIcon.into());

        view.append_column(&url_column);

        Self {
            view,
            column,
            check_renderer,
            url_column,
            description_column,
        }
    }

    fn iter_of_clicked_url(&self, event: &gdk::EventButton) -> Option<gtk::TreeIter> {
        let (x, y) = event.coords()?;
        let (path, col, cell_x, _cell_y) = self.view.path_at_pos(x as i32, y as i32)?;
        let path = path?;
        let col = col?;
        if col != self.url_column {
            return None;
        }

        let clicked_icon = self
            .url_column
            .cells()
            .into_iter()
            .filter(|cell| cell.downcast_ref::<gtk::CellRendererPixbuf>().is_some())
            .filter_map(|cell| self.url_column.cell_get_position(&cell))
            .filter(|(cell_pos, cell_width)| *cell_pos <= cell_x && cell_x <= cell_pos + cell_width)
            .next()
            .is_some();

        if clicked_icon {
            let (_model, iter) = get_real_iter(&self.view, &path)?;
            return Some(iter);
        }
        None
    }

    pub fn connect_url_clicked<F: Fn(gtk::TreeIter) + 'static>(&self, handler: F) {
        self.view.connect_button_release_event(
            clone!(@weak self as this => @default-return glib::signal::Inhibit(false), move |_view, event| {
                if let Some(iter) = this.iter_of_clicked_url(event) {
                    handler(iter);
                }
                glib::signal::Inhibit(false)
            }),
        );
    }

    pub fn connect_drop<F: Fn(gtk::TreeIter) -> bool + 'static>(&self, is_group: F) {
        self.view
            .connect_drag_motion(move |view, drag_context, x, y, time| {
                if let Some((Some(path), pos)) = view.dest_row_at_pos(x, y) {
                    if pos == gtk::TreeViewDropPosition::IntoOrBefore
                        || pos == gtk::TreeViewDropPosition::IntoOrAfter
                    {
                        if let Some((_model, iter)) = get_real_iter(view, &path) {
                            if !is_group(iter) {
                                drag_context.drag_status(gdk::DragAction::empty(), time); // deny
                                return true; // stop propagation
                            }
                        }
                    }
                }
                drag_context.drag_status(gdk::DragAction::MOVE, time);
                false
            });
    }

    pub fn connect_cursor_changed<F: Fn(Option<(gtk::TreeIter, gtk::TreePath)>) + 'static>(
        &self,
        changed: F,
    ) {
        self.view.connect_cursor_changed(move |view| {
            changed(get_selected_iter(view));
        });
    }

    pub fn connect_row_activated<F: Fn(Option<(gtk::TreeIter, gtk::TreePath)>) + 'static>(
        &self,
        activated: F,
    ) {
        self.view.connect_row_activated(move |view, _iter, _col| {
            activated(get_selected_iter(view));
        });
    }

    pub fn set_popup(&self, popup_model: &gio::MenuModel) {
        let popup = gtk::Menu::from_model(popup_model);
        popup.set_attach_widget(Some(&self.view));

        self.view.connect_button_press_event(
            clone!(@weak popup => @default-return Inhibit(false), move |view, event| {
                let button = event.button();
                if button == GDK_BUTTON_SECONDARY {
                    view.grab_focus();

                    let (x, y) = event.position();
                    if let Some((Some(path), _, _, _)) = view.path_at_pos(x as i32, y as i32) {
                        view.set_cursor(&path, None::<&gtk::TreeViewColumn>, false);
                    }

                    popup.popup_at_pointer(Some(&*event));

                    Inhibit(true)
                } else {
                    Inhibit(false)
                }
            }),
        );

        self.view
            .connect_popup_menu(clone!(@weak popup => @default-return false, move |view| {
                view.grab_focus();
                popup.popup_at_widget(view, gdk::Gravity::Center, gdk::Gravity::Center, None);
                true
            }));
    }

    pub fn toggle_group(&self, path: &gtk::TreePath) {
        if self.view.row_expanded(path) {
            self.view.collapse_row(path);
        } else {
            self.view.expand_row(path, false);
        }
    }

    pub fn get_widget(&self) -> gtk::Widget {
        self.view.clone().upcast()
    }

    pub fn set_model(&self, model: &gtk::TreeModel) {
        self.view.set_model(Some(model));
    }

    pub fn get_selected_iter(&self) -> Option<(gtk::TreeIter, gtk::TreePath)> {
        get_selected_iter(&self.view)
    }

    pub fn select_iter(&self, iter: &gtk::TreeIter) {
        select_iter(&self.view, iter);
    }

    pub fn set_selection_mode(&self, selection: bool) {
        if selection {
            self.column.add_attribute(
                &self.check_renderer,
                "visible",
                TreeStoreColumn::SelectionVisible.into(),
            );
            self.column.add_attribute(
                &self.check_renderer,
                "active",
                TreeStoreColumn::Selection.into(),
            );
        } else {
            self.column.clear_attributes(&self.check_renderer);
            self.check_renderer.set_visible(false);
        }
        self.column.queue_resize();
    }
}

fn selection_toggled(view: &gtk::TreeView, path: &gtk::TreePath) {
    if let Some((model, iter)) = get_real_iter(view, path) {
        if let Ok(store) = model.downcast::<gtk::TreeStore>() {
            let selected: bool = store
                .value(&iter, TreeStoreColumn::Selection.into())
                .get()
                .unwrap_or(false);
            store.set_value(
                &iter,
                TreeStoreColumn::Selection.into(),
                &glib::Value::from(&!selected),
            );
        }
    }
}
