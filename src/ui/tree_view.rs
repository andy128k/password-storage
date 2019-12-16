use glib::{Value};
use gio::MenuModel;
use gdk::{DragAction, Event};
use gtk::prelude::*;
use gtk::{
    TreeView, TreeViewExt, Widget, WidgetExt,
    TreeViewColumn, TreeViewColumnSizing, CellRendererToggle, CellRendererPixbuf, CellRendererText,
    TreeModelFilter, TreeModelFilterExt, TreeIter, TreePath, TreeModel, TreeStore,
    TreeViewDropPosition, IconSize,
};
use crate::store::TreeStoreColumn;
use crate::utils::menu::MenuExtManual;

const GDK_BUTTON_SECONDARY: u32 = 3;

#[derive(Clone)]
pub struct PSTreeView {
    pub view: TreeView,
    column: TreeViewColumn,
    check_renderer: CellRendererToggle,
}

fn get_real_iter(view: &TreeView, path: &TreePath) -> Option<(TreeModel, TreeIter)> {
    let model = view.get_model()?;
    let iter = model.get_iter(path)?;
    match model.clone().downcast::<TreeModelFilter>() {
        Ok(filter) => Some((filter.get_model().unwrap(), filter.convert_iter_to_child_iter(&iter))),
        Err(_) => Some((model, iter))
    }
}

pub fn get_selected_iter(view: &TreeView) -> Option<(TreeIter, TreePath)> {
    let (current_model, iter) = view.get_selection().get_selected()?;
    let path = current_model.get_path(&iter)?;
    let real_iter = match current_model.downcast::<TreeModelFilter>() {
        Ok(filter) => filter.convert_iter_to_child_iter(&iter),
        Err(_) => iter
    };
    Some((real_iter, path))
}

pub fn select_iter(view: &TreeView, iter: &TreeIter) {
    if let Some(current_model) = view.get_model() {
        let iter_to_select = match current_model.clone().downcast::<TreeModelFilter>() {
            Ok(filter) => filter.convert_child_iter_to_iter(iter),
            Err(_) => Some(iter.clone())
        };
        if let Some(path) = iter_to_select.and_then(|iter| current_model.get_path(&iter)) {
            view.expand_to_path(&path);
            view.set_cursor(&path, None::<&TreeViewColumn>, false);
        }
    }
}

impl PSTreeView {
    pub fn new() -> PSTreeView {
        let view = TreeView::new();
        view.set_can_focus(true);
        view.set_headers_visible(false);
        view.set_reorderable(true);
        view.set_search_column(1);

        let column = TreeViewColumn::new();
        column.set_sizing(TreeViewColumnSizing::Autosize);
        column.set_expand(true);

        let check_renderer = CellRendererToggle::new();
        check_renderer.set_visible(false);
        {
            let view1 = view.clone();
            check_renderer.connect_toggled(move |_renderer, path| selection_toggled(&view1, &path));
        }
        column.pack_start(&check_renderer, false);

        let icon = CellRendererPixbuf::new();
        icon.set_property_stock_size(IconSize::Menu);
        column.pack_start(&icon, false);
        column.add_attribute(&icon, "icon-name", TreeStoreColumn::TypeIcon.into());

        let text = CellRendererText::new();
        column.pack_start(&text, true);
        column.add_attribute(&text, "text", TreeStoreColumn::Name.into());

        view.append_column(&column);

        view.append_column(&{
            let column = TreeViewColumn::new();
            column.set_sizing(TreeViewColumnSizing::Fixed);

            let strength = CellRendererPixbuf::new();
            strength.set_property_stock_size(IconSize::Menu);
            strength.set_padding(16, 0);
            column.pack_start(&strength, false);
            column.add_attribute(&strength, "icon-name", TreeStoreColumn::Strength.into());

            column
        });

        PSTreeView { view, column, check_renderer }
    }

    pub fn connect_drop<F: Fn(TreeIter) -> bool + 'static>(&self, is_group: F) {
        self.view.connect_drag_motion(move |view, drag_context, x, y, time| {
            if let Some((Some(path), pos)) = view.get_dest_row_at_pos(x, y) {
                if pos == TreeViewDropPosition::IntoOrBefore || pos == TreeViewDropPosition::IntoOrAfter {
                    if let Some((_model, iter)) = get_real_iter(view, &path) {
                        if !is_group(iter) {
                            drag_context.drag_status(DragAction::empty(), time); // deny
                            return Inhibit(true); // stop propagation
                        }
                    }
                }
            }
            drag_context.drag_status(DragAction::MOVE, time);
            Inhibit(false)
        });
    }

    pub fn connect_cursor_changed<F: Fn(Option<(TreeIter, TreePath)>) + 'static>(&self, changed: F) {
        self.view.connect_cursor_changed(move |view| {
            changed(get_selected_iter(view));
        });
    }

    pub fn connect_row_activated<F: Fn(Option<(TreeIter, TreePath)>) + 'static>(&self, activated: F) {
        self.view.connect_row_activated(move |view, _iter, _col| {
            activated(get_selected_iter(view));
        });
    }

    pub fn set_popup(&self, popup_model: &MenuModel) {
        let popup = gtk::Menu::new_from_model(&popup_model.clone());
        popup.attach_to_widget_no_detacher(&self.view);

        let popup1 = popup.clone();
        self.view.connect_button_press_event(move |view, event| {
            let button = event.get_button();
            if button == GDK_BUTTON_SECONDARY {
                view.grab_focus();

                let (x, y) = event.get_position();
                if let Some((Some(path), _, _, _)) = view.get_path_at_pos(x as i32, y as i32) {
                    view.set_cursor(&path, None::<&TreeViewColumn>, false);
                }

                let base_event: &Event = event;
                popup1.popup_at_pointer(Some(base_event));

                Inhibit(true)
            } else {
                Inhibit(false)
            }
        });

        let popup2 = popup.clone();
        self.view.connect_popup_menu(move |view| {
            view.grab_focus();
            popup2.popup_at_widget(view, gdk::Gravity::Center, gdk::Gravity::Center, None);
            true
        });
    }

    pub fn toggle_group(&self, path: &TreePath) {
        if self.view.row_expanded(path) {
            self.view.collapse_row(path);
        } else {
            self.view.expand_row(path, false);
        }
    }

    pub fn get_widget(&self) -> Widget {
        self.view.clone().upcast()
    }

    pub fn set_model(&self, model: Option<&TreeModel>) {
        self.view.set_model(model);
    }

    pub fn get_selected_iter(&self) -> Option<(TreeIter, TreePath)> {
        get_selected_iter(&self.view)
    }

    pub fn select_iter(&self, iter: &TreeIter) {
        select_iter(&self.view, iter);
    }

    pub fn set_selection_mode(&self, selection: bool) {
        if selection {
            self.column.add_attribute(&self.check_renderer, "visible", TreeStoreColumn::SelectionVisible.into());
            self.column.add_attribute(&self.check_renderer, "active", TreeStoreColumn::Selection.into());
            self.column.queue_resize();
        } else {
            self.column.clear_attributes(&self.check_renderer);
            self.check_renderer.set_visible(false);
            self.column.queue_resize();
        }
    }
}

fn selection_toggled(view: &TreeView, path: &TreePath) {
    if let Some((model, iter)) = get_real_iter(view, path) {
        if let Ok(store) = model.downcast::<TreeStore>() {
            let selected: bool = store.get_value(&iter, TreeStoreColumn::Selection.into()).get().unwrap().unwrap_or(false);
            store.set_value(&iter, TreeStoreColumn::Selection.into(), &Value::from(&!selected));
        }
    }
}
