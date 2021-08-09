use crate::cache::Cache;
use crate::config::Config;
use crate::error::*;
use crate::format;
use crate::gtk_prelude::*;
use crate::model::record::{Record, RecordType, FIELD_NAME, RECORD_TYPE_GENERIC};
use crate::model::tree::RecordTree;
use crate::store::PSStore;
use crate::ui;
use crate::ui::dashboard::PSDashboard;
use crate::ui::dialogs::ask::{confirm_likely, confirm_unlikely};
use crate::ui::dialogs::ask_save::{ask_save, AskSave};
use crate::ui::dialogs::change_password::change_password;
use crate::ui::dialogs::file_chooser::{open_file, save_file};
use crate::ui::dialogs::read_file::read_file;
use crate::ui::dialogs::say::{say_error, say_info};
use crate::ui::edit_record::edit_record;
use crate::ui::filter::create_model_filter;
use crate::ui::preview_panel::PSPreviewPanel;
use crate::ui::search::create_search_entry;
use crate::ui::tree_view::PSTreeView;
use crate::utils::clipboard::get_clipboard;
use crate::utils::string::StringExt;
use crate::utils::ui::*;
use guard::guard;
use once_cell::unsync::OnceCell;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::convert::Into;
use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::rc::Rc;

const WINDOW_TITLE: &str = "Password Storage";

enum AppMode {
    Initial,
    FileOpened,
    MergeMode,
}

struct PSMainWindowPrivate {
    mode: Cell<AppMode>,
    stack: gtk::Stack,
    dashboard: PSDashboard,
    data: RefCell<PSStore>,
    view: PSTreeView,
    preview: PSPreviewPanel,

    doc_actions: gio::SimpleActionGroup,
    file_actions: gio::SimpleActionGroup,
    merge_actions: gio::SimpleActionGroup,
    entry_actions: gio::SimpleActionGroup,

    search_entry: gtk::Entry,

    filename: RefCell<Option<PathBuf>>,
    password: RefCell<Option<String>>,
    changed: Cell<bool>,

    config: OnceCell<Rc<RefCell<Config>>>,
    cache: OnceCell<Cache>,
}

#[derive(Default)]
pub struct PSMainWindowInner {
    private: OnceCell<PSMainWindowPrivate>,
    statusbar: OnceCell<gtk::Statusbar>,
    delete_handler: RefCell<Option<glib::signal::SignalHandlerId>>,
}

#[glib::object_subclass]
impl ObjectSubclass for PSMainWindowInner {
    const NAME: &'static str = "PSMainWindow";
    type Type = PSMainWindow;
    type ParentType = gtk::ApplicationWindow;
}

impl ObjectImpl for PSMainWindowInner {
    fn constructed(&self, win: &Self::Type) {
        self.parent_constructed(win);

        win.set_title(WINDOW_TITLE);
        win.set_icon_name(Some("password-storage"));
        win.set_window_position(gtk::WindowPosition::Center);
        win.set_default_width(1000);
        win.set_default_height(800);

        let data = PSStore::new();
        let view = PSTreeView::new();
        let preview = PSPreviewPanel::new();

        let dashboard = PSDashboard::new();

        let search_entry = create_search_entry();
        let toolbar = ui::toolbar::create_tool_bar(&search_entry.clone().upcast());
        let statusbar = gtk::Statusbar::new();

        let stack = gtk::Stack::new()
            .named("dashboard", &dashboard.get_widget())
            .named(
                "file",
                &paned(&scrolled(&view.get_widget()), &preview.get_widget()),
            );

        let grid = {
            let grid = gtk::Grid::new();

            toolbar.set_halign(gtk::Align::Fill);
            toolbar.set_valign(gtk::Align::Start);
            grid.attach(&toolbar, 0, 0, 1, 1);

            grid.attach(&stack, 0, 1, 1, 1);

            statusbar.set_halign(gtk::Align::Fill);
            statusbar.set_valign(gtk::Align::End);
            grid.attach(&statusbar, 0, 2, 1, 1);

            grid
        };

        win.add(&grid);

        view.set_model(&data.as_model());

        let doc_actions = gio::SimpleActionGroup::new();
        win.register_doc_actions(&doc_actions);

        let file_actions = gio::SimpleActionGroup::new();
        win.register_file_actions(&file_actions);

        let merge_actions = gio::SimpleActionGroup::new();
        win.register_merge_actions(&merge_actions);

        let entry_actions = gio::SimpleActionGroup::new();
        win.register_entry_actions(&entry_actions);

        let private = PSMainWindowPrivate {
            mode: Cell::new(AppMode::Initial),
            stack,
            dashboard,
            data: RefCell::new(data),
            view,
            search_entry,
            preview,

            doc_actions: doc_actions.clone(),
            file_actions: file_actions.clone(),
            merge_actions: merge_actions.clone(),
            entry_actions: entry_actions.clone(),

            filename: RefCell::new(None),
            password: RefCell::new(None),
            changed: Cell::new(false),

            config: OnceCell::new(),
            cache: OnceCell::new(),
        };

        self.private
            .set(private)
            .ok()
            .expect("private is set only once");

        self.statusbar
            .set(statusbar)
            .ok()
            .expect("statusbar is set only once");

        let delete_handler = win.connect_delete_event(
            clone!(@weak win => @default-return gtk::Inhibit(false), move |_win, _event| {
                glib::MainContext::default().spawn_local(async move {
                    win.on_close().await;
                });
                gtk::Inhibit(true)
            }),
        );
        *self.delete_handler.borrow_mut() = Some(delete_handler);

        win.insert_action_group("doc", Some(&doc_actions));
        win.insert_action_group("file", Some(&file_actions));
        win.insert_action_group("merge", Some(&merge_actions));
        win.insert_action_group("entry", Some(&entry_actions));

        win.private()
            .search_entry
            .connect_changed(clone!(@weak win => move |search_entry| {
                if let Some(search_text) = search_entry.text().non_empty() {
                    let look_at_secrets = win.private().config.get().unwrap().borrow().search_in_secrets;
                    win.set_status("View is filtered.");
                    let model = create_model_filter(
                        &win.private().data.borrow(),
                        &search_text,
                        look_at_secrets,
                    );
                    win.private().view.set_model(&model.upcast());
                    win.private().view.view.set_reorderable(false);
                    win.refilter();
                    win.private().view.view.expand_all();
                } else {
                    win.set_status("View filter was reset.");
                    let model = win.private().data.borrow().as_model();
                    win.private().view.set_model(&model);
                    win.private().view.view.set_reorderable(true);
                    win.private().view.view.collapse_all();
                }
                win.listview_cursor_changed(None);
            }));

        win.private()
            .view
            .connect_cursor_changed(clone!(@weak win => move |selection| {
                let mut record = None;
                if let Some((iter, _path)) = selection {
                    record = win.private().data.borrow().get(&iter);
                }
                win.listview_cursor_changed(record);
            }));

        win.private()
            .view
            .connect_drop(clone!(@weak win => @default-return false, move |iter| {
                let record_opt = win.private().data.borrow().get(&iter);
                if let Some(record) = record_opt {
                    record.record_type.is_group
                } else {
                    false
                }
            }));

        win.private()
            .view
            .connect_row_activated(clone!(@weak win => move |selection| {
                glib::MainContext::default().spawn_local(async move {
                    win.on_row_activated(selection).await;
                });
            }));

        let popup = ui::menu::create_tree_popup();
        win.private().view.set_popup(&popup);
    }
}

impl WidgetImpl for PSMainWindowInner {}
impl ContainerImpl for PSMainWindowInner {}
impl BinImpl for PSMainWindowInner {}
impl WindowImpl for PSMainWindowInner {}
impl ApplicationWindowImpl for PSMainWindowInner {}

glib::wrapper! {
    pub struct PSMainWindow(ObjectSubclass<PSMainWindowInner>)
        @extends gtk::ApplicationWindow, gtk::Window, gtk::Bin, gtk::Container, gtk::Widget, @implements gio::ActionMap;
}

impl PSMainWindow {
    pub fn from_window(window: &gtk::Window) -> Option<Self> {
        window.clone().downcast::<Self>().ok()
    }

    fn private(&self) -> &PSMainWindowPrivate {
        let private = PSMainWindowInner::from_instance(self);
        private.private.get().unwrap()
    }

    fn set_status(&self, message: &str) {
        let private = PSMainWindowInner::from_instance(self);
        if let Some(statusbar) = private.statusbar.get() {
            statusbar.pop(0);
            statusbar.push(0, message);
        }
    }

    async fn ensure_data_is_saved(&self) -> bool {
        if !self.private().changed.get() {
            return true;
        }
        let window = self.clone().upcast();
        match ask_save(
            &window,
            "Save changes before closing? If you don't save, changes will be permanently lost.",
        )
        .await
        {
            AskSave::Save => self.cb_save().await,
            AskSave::Discard => true,
            AskSave::Cancel => false,
        }
    }

    fn refilter(&self) {
        if let Some(model) = self.private().view.view.model() {
            if let Ok(filter) = model.downcast::<gtk::TreeModelFilter>() {
                filter.refilter();
            }
        }
    }

    fn get_selected_group_iter(&self) -> Option<gtk::TreeIter> {
        let model = &self.private().data.borrow();
        let selection = self.private().view.get_selected_iter();
        if let Some((iter, _path)) = selection {
            for (i, record) in model.parents(&iter) {
                if record.record_type.is_group {
                    return Some(i);
                }
            }
        }
        None
    }

    fn listview_cursor_changed(&self, record: Option<Record>) {
        let entry_actions = &self.private().entry_actions;
        let is_selected_record = record.is_some();

        entry_actions
            .simple_action("copy-name")
            .set_enabled(is_selected_record);
        entry_actions
            .simple_action("copy-password")
            .set_enabled(record.as_ref().and_then(|e| e.password()).is_some());
        entry_actions
            .simple_action("edit")
            .set_enabled(is_selected_record);
        entry_actions
            .simple_action("delete")
            .set_enabled(is_selected_record);
        entry_actions
            .simple_action("convert-to")
            .set_enabled(record.as_ref().map_or(false, |r| !r.record_type.is_group));

        let show_secrets_on_preview = self
            .private()
            .config
            .get()
            .unwrap()
            .borrow()
            .show_secrets_on_preview;
        self.private()
            .preview
            .update(record, show_secrets_on_preview);
    }

    fn get_usernames(&self) -> Vec<String> {
        fn traverse(
            store: &PSStore,
            parent_iter: Option<&gtk::TreeIter>,
            result: &mut HashSet<String>,
        ) {
            for (i, record) in store.children(parent_iter) {
                if record.record_type.is_group {
                    traverse(store, Some(&i), result);
                } else {
                    record
                        .username()
                        .map(|username| result.insert(username.to_string()));
                }
            }
        }

        let mut result = HashSet::new();
        traverse(&self.private().data.borrow(), None, &mut result);
        result.remove("");

        let mut vec: Vec<String> = result.into_iter().collect();
        vec.sort();
        vec
    }

    async fn ensure_password_is_set(&self) -> Option<String> {
        if self.private().password.borrow().is_none() {
            *self.private().password.borrow_mut() = new_password(&self.clone().upcast()).await;
        }
        self.private().password.borrow().clone()
    }

    async fn save_data(&self, filename: &Path) -> Result<()> {
        if let Some(password) = self.ensure_password_is_set().await {
            let tree = self.private().data.borrow().to_tree();

            format::save_file(filename, &password, &tree)?;

            self.set_status(&format!("File '{}' was saved", filename.display()));
            self.private().changed.set(false);
            self.private().cache.get().unwrap().add_file(filename);
        }
        Ok(())
    }

    fn set_filename(&self, filename: Option<&Path>) {
        match filename {
            Some(filename) => {
                *self.private().filename.borrow_mut() = Some(filename.to_owned());
                self.set_title(&format!("{} - {}", WINDOW_TITLE, &filename.display()));
            }
            None => {
                *self.private().filename.borrow_mut() = None;
                self.set_title(WINDOW_TITLE);
            }
        }
    }

    pub async fn new_file(&self) {
        if self.ensure_data_is_saved().await {
            self.private().changed.set(false);

            let data = PSStore::new();
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            self.set_filename(None);
            self.private().search_entry.set_text("");
            *self.private().password.borrow_mut() = None;
            self.listview_cursor_changed(None);
            self.set_status("New file was created");
            self.set_mode(AppMode::FileOpened);
        }
    }

    pub async fn do_open_file(&self, filename: &Path) {
        if let Some((entries, password)) =
            load_data(filename.to_owned(), &self.clone().upcast()).await
        {
            self.private().cache.get().unwrap().add_file(filename);
            self.private().search_entry.set_text("");

            let data = PSStore::from_tree(&entries);
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            {
                self.set_filename(Some(filename));
                *self.private().password.borrow_mut() = Some(password);
                self.private().changed.set(false);
            }
            self.set_mode(AppMode::FileOpened);
            self.set_status(&format!("File '{}' was opened", filename.display()));
            self.private().search_entry.grab_focus();
        }
    }

    pub async fn open_file(&self) {
        if self.ensure_data_is_saved().await {
            if let Some(filename) = open_file(&self.clone().upcast()).await {
                self.do_open_file(&filename).await;
            }
        }
    }

    async fn cb_save_as(&self) -> bool {
        let window = self.clone().upcast();
        if let Some(ref filename) = save_file(&window).await {
            self.set_filename(Some(filename));
            if let Err(error) = self.save_data(filename).await {
                say_error(&window, &error.to_string()).await;
                return false;
            }
        }
        true
    }

    async fn cb_save(&self) -> bool {
        let filename = self.private().filename.borrow().clone();
        if let Some(ref filename) = filename {
            if let Err(error) = self.save_data(filename).await {
                let window = self.clone().upcast();
                say_error(&window, &error.to_string()).await;
                return false;
            }
            true
        } else {
            self.cb_save_as().await
        }
    }

    async fn on_row_activated(&self, selection: Option<(gtk::TreeIter, gtk::TreePath)>) {
        if let Some((iter, path)) = selection {
            let record_opt = self.private().data.borrow().get(&iter);
            if let Some(record) = record_opt {
                if record.record_type.is_group {
                    self.private().view.toggle_group(&path);
                } else {
                    self.action_edit().await;
                }
            }
        }
    }

    fn set_mode(&self, mode: AppMode) {
        let private = self.private();
        match mode {
            AppMode::Initial => {
                private.doc_actions.set_enabled(false);
                private.file_actions.set_enabled(false);
                private.merge_actions.set_enabled(false);
                private.entry_actions.set_enabled(false);

                private
                    .doc_actions
                    .simple_action("merge-mode")
                    .set_state(&false.to_variant());

                self.private().search_entry.set_sensitive(false);
                self.private().view.set_selection_mode(false);
                self.private().stack.set_visible_child_name("dashboard");
                if let Some(cache) = self.private().cache.get() {
                    self.private().dashboard.update(cache);
                }
            }
            AppMode::FileOpened => {
                private.doc_actions.set_enabled(true);
                private.file_actions.set_enabled(true);
                private.merge_actions.set_enabled(false);
                private.entry_actions.set_enabled(true);

                private
                    .doc_actions
                    .simple_action("merge-mode")
                    .set_state(&false.to_variant());

                self.private().search_entry.set_sensitive(true);
                self.private().view.set_selection_mode(false);
                self.private().stack.set_visible_child_name("file");
            }
            AppMode::MergeMode => {
                private.doc_actions.set_enabled(true);
                private.file_actions.set_enabled(false);
                private.merge_actions.set_enabled(true);
                private.entry_actions.set_enabled(false);

                private
                    .doc_actions
                    .simple_action("merge-mode")
                    .set_state(&true.to_variant());

                self.private().search_entry.set_sensitive(true);
                self.private().view.set_selection_mode(true);
                self.private().stack.set_visible_child_name("file");
            }
        }
        self.private().mode.set(mode);
    }

    fn set_merge_mode(&self, merge: bool) {
        self.set_mode(if merge {
            AppMode::MergeMode
        } else {
            AppMode::FileOpened
        });
    }

    async fn on_close(&self) {
        if self.ensure_data_is_saved().await {
            get_clipboard().clear();

            let private = PSMainWindowInner::from_instance(self);
            if let Some(handler) = private.delete_handler.take() {
                self.disconnect(handler);
            }
            self.close();
        }
    }

    pub fn new(app: &gtk::Application, config: &Rc<RefCell<Config>>, cache: &Cache) -> Self {
        let win = glib::Object::new(&[("application", app)]).expect("MainWindow is created");

        let private = PSMainWindowInner::from_instance(&win);
        private
            .private
            .get()
            .unwrap()
            .config
            .set(config.clone())
            .ok()
            .unwrap();
        private
            .private
            .get()
            .unwrap()
            .cache
            .set(cache.clone())
            .ok()
            .unwrap();
        private.private.get().unwrap().dashboard.update(cache);

        win.set_mode(AppMode::Initial);

        win
    }
}

#[awesome_glib::actions(register_fn = "register_doc_actions")]
impl PSMainWindow {
    #[action(stateful, name = "merge-mode")]
    fn action_merge_mode(&self, toggled: bool) -> Option<bool> {
        self.set_merge_mode(!toggled);
        Some(!toggled)
    }
}

#[awesome_glib::actions(register_fn = "register_file_actions")]
impl PSMainWindow {
    #[action(name = "close")]
    async fn action_close_file(&self) {
        if self.ensure_data_is_saved().await {
            self.private().changed.set(false);
            self.private().search_entry.set_text("");

            let data = PSStore::new();
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            self.set_filename(None);
            *self.private().password.borrow_mut() = None;
            self.listview_cursor_changed(None);
            self.set_mode(AppMode::Initial);
            self.set_status("");
        }
    }

    #[action(name = "save")]
    async fn action_save(&self) {
        let _saved = self.cb_save().await;
    }

    #[action(name = "save-as")]
    async fn action_save_as(&self) {
        let _saved = self.cb_save_as().await;
    }

    #[action(name = "merge-file")]
    async fn action_merge_file(&self) {
        self.private().search_entry.set_text("");

        let window = self.clone().upcast();
        if let Some(filename) = open_file(&window).await {
            if let Some((extra_records, _password)) = load_data(filename, &window).await {
                let mut records_tree = self.private().data.borrow().to_tree();
                crate::model::merge_trees::merge_trees(&mut records_tree, &extra_records);

                let data = PSStore::from_tree(&records_tree);
                *self.private().data.borrow_mut() = data.clone();
                self.private().view.set_model(&data.as_model());

                self.private().changed.set(true);
            }
        }
    }

    #[action(name = "change-password")]
    async fn action_change_password(&self) {
        if let Some(new_password) = change_password(&self.clone().upcast()).await {
            *self.private().password.borrow_mut() = Some(new_password);
            self.private().changed.set(true);
            self.set_status("File password was changed");
        }
    }

    #[action(name = "find")]
    fn action_find(&self) {
        self.private().search_entry.grab_focus();
    }

    #[action(name = "add")]
    async fn action_add_record(&self, record_type_name: String) {
        let record_type = RecordType::find(&record_type_name).unwrap_or(&*RECORD_TYPE_GENERIC);

        let empty_record = record_type.new_record();
        if let Some(new_record) = edit_record(
            &empty_record,
            &self.clone().upcast(),
            "Add",
            &self.get_usernames(),
        )
        .await
        {
            let group_iter = self.get_selected_group_iter();
            let iter = self
                .private()
                .data
                .borrow()
                .append(group_iter.as_ref(), &new_record);
            self.refilter();
            self.private().view.select_iter(&iter);
            self.set_status("New entry was added");
            self.private().changed.set(true);
        }
    }
}

#[awesome_glib::actions(register_fn = "register_merge_actions")]
impl PSMainWindow {
    #[action(name = "uncheck-all")]
    fn action_uncheck_all(&self) {
        self.private().data.borrow().uncheck_all();
        self.set_status("Unchecked all items");
    }

    #[action(name = "merge")]
    async fn action_merge(&self) {
        let checked = {
            fn collect_checked(
                model: &PSStore,
                parent: Option<&gtk::TreeIter>,
                path: &[String],
                result: &mut Vec<(Record, Vec<String>)>,
            ) {
                for (i, record) in model.children(parent) {
                    if model.is_selected(&i) {
                        result.push((record.clone(), path.to_vec()));
                    }
                    let mut p = path.to_vec();
                    p.push(record.name().to_string());
                    collect_checked(model, Some(&i), &p, result);
                }
            }
            let mut checked = Vec::new();
            collect_checked(
                &self.private().data.borrow(),
                None,
                &Vec::new(),
                &mut checked,
            );
            checked
        };

        if checked.len() < 2 {
            say_info(
                &self.clone().upcast(),
                "Nothing to merge. Select few items and try again.",
            )
            .await;
            return;
        }

        // rename
        let records = {
            let mut renamed_records: Vec<Record> = Vec::new();
            for &(ref record, ref path) in &checked {
                let mut renamed = record.clone();
                renamed.rename(move |old_name| format!("{}/{}", path.join("/"), old_name));
                renamed_records.push(renamed);
            }
            renamed_records
        };

        {
            let mut message = String::from("Do you want to merge following items?\n");
            for record in &records {
                message.push('\n');
                message.push_str(&record.name());
            }

            if !confirm_likely(&self.clone().upcast(), &message).await {
                return;
            }
        }

        // delete entries
        self.private().data.borrow().delete_checked();

        // create new entry
        let result = Record::join_entries(&records);

        // TODO: detect common path
        let iter = self.private().data.borrow().append(None, &result);
        self.refilter();
        self.private().view.select_iter(&iter);
        self.set_status("New entry was created by merging");
        self.private().changed.set(true);
    }
}

#[awesome_glib::actions(register_fn = "register_entry_actions")]
impl PSMainWindow {
    #[action(name = "copy-name")]
    fn action_copy_name(&self) {
        if let Some((iter, _path)) = self.private().view.get_selected_iter() {
            if let Some(record) = self.private().data.borrow().get(&iter) {
                if let Some(username) = record.username() {
                    get_clipboard().set_text(username);
                    self.set_status("Name was copied to clipboard");
                }
            }
        }
    }

    #[action(name = "copy-password")]
    fn action_copy_password(&self) {
        if let Some((iter, _path)) = self.private().view.get_selected_iter() {
            if let Some(record) = self.private().data.borrow().get(&iter) {
                if let Some(password) = record.password() {
                    get_clipboard().set_text(password);
                    self.set_status("Secret (password) was copied to clipboard");
                }
            }
        }
    }

    #[action(name = "edit")]
    async fn action_edit(&self) {
        let selection = self.private().view.get_selected_iter();
        if let Some((iter, _path)) = selection {
            let record_opt = self.private().data.borrow().get(&iter);
            if let Some(record) = record_opt {
                if let Some(new_record) = edit_record(
                    &record,
                    &self.clone().upcast(),
                    "Edit",
                    &self.get_usernames(),
                )
                .await
                {
                    self.private().data.borrow().update(&iter, &new_record);
                    self.refilter();
                    self.listview_cursor_changed(Some(new_record));
                    self.set_status("Entry was changed");
                    self.private().changed.set(true);
                }
            }
        }
    }

    #[action(name = "delete")]
    async fn action_delele(&self) {
        guard!(let Some(selection) = self.private().view.get_selected_iter() else { return });
        let confirmed = confirm_unlikely(
            &self.clone().upcast(),
            "Do you really want to delete selected entry?",
        )
        .await;
        if confirmed {
            self.private().data.borrow().delete(&selection.0);
            self.listview_cursor_changed(None);
            self.set_status("Entry was deleted");
            self.private().changed.set(true);
        }
    }

    #[action(name = "convert-to")]
    fn action_convert(&self, dest_record_type_name: String) {
        guard!(let Some(selection) = self.private().view.get_selected_iter() else { return; });
        let selection_iter = selection.0;
        guard!(let Some(record) = self.private().data.borrow().get(&selection_iter) else { return; });
        if record.record_type.is_group {
            return;
        }

        guard!(let Some(dest_record_type) = RecordType::find(&dest_record_type_name)
            .filter(|rt| !rt.is_group && !rt.ref_eq(record.record_type)) else { return; });

        let new_record = {
            let mut new_record = dest_record_type.new_record();
            let name = record.get_field(&FIELD_NAME);
            new_record.set_field(&FIELD_NAME, name);
            new_record.join(&record);
            new_record
        };
        self.private()
            .data
            .borrow()
            .update(&selection_iter, &new_record);
        self.refilter();
        self.set_status("Entry has changed type");
        self.private().changed.set(true);
        self.listview_cursor_changed(Some(new_record));
    }
}

async fn load_data(filename: PathBuf, parent_window: &gtk::Window) -> Option<(RecordTree, String)> {
    read_file(parent_window, move |password| {
        format::load_file(&filename, password)
    })
    .await
}

async fn new_password(parent_window: &gtk::Window) -> Option<String> {
    // TODO: ADD confirmation
    let mut form = ui::forms::form::Form::new();
    form.add(
        "Password",
        Box::new(ui::forms::entry::Password::new()),
        true,
    );
    let result = ui::edit_object::edit_object(
        None,
        form,
        parent_window,
        "Enter password",
        "password-storage",
    )
    .await;
    result.map(|mut values| values.remove(0))
}
