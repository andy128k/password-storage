use crate::cache::Cache;
use crate::config::ConfigService;
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
use crate::ui::menu::create_add_entity_menu;
use crate::ui::merge_bar::create_merge_bar;
use crate::ui::preview_panel::PSPreviewPanel;
use crate::ui::search::create_search_bar;
use crate::ui::search::SearchEvent;
use crate::ui::toast::Toast;
use crate::ui::tree_view::PSTreeView;
use crate::utils::clipboard::get_clipboard;
use crate::utils::string::StringExt;
use crate::utils::tree::flatten_tree;
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

#[derive(Clone, Copy)]
enum AppMode {
    Initial,
    FileOpened,
    MergeMode,
}

struct PSMainWindowPrivate {
    mode: Cell<AppMode>,
    header_bar: gtk::HeaderBar,
    stack: gtk::Stack,
    dashboard: PSDashboard,
    data: RefCell<PSStore>,
    view: PSTreeView,
    preview: PSPreviewPanel,

    doc_actions: gio::SimpleActionGroup,
    file_actions: gio::SimpleActionGroup,
    merge_actions: gio::SimpleActionGroup,
    entry_actions: gio::SimpleActionGroup,

    search_entry: gtk::SearchEntry,
    search_bar: gtk::SearchBar,
    merge_bar: gtk::InfoBar,
    toast: Toast,

    filename: RefCell<Option<PathBuf>>,
    password: RefCell<Option<String>>,
    changed: Cell<bool>,

    config: OnceCell<Rc<ConfigService>>,
    cache: OnceCell<Cache>,
}

#[derive(Default)]
pub struct PSMainWindowInner {
    private: OnceCell<PSMainWindowPrivate>,
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

        let header_bar = gtk::HeaderBar::builder()
            .show_close_button(true)
            .title(WINDOW_TITLE)
            .build();
        win.set_titlebar(Some(&header_bar));

        let open_button = gtk::Button::builder()
            .label("Open")
            .action_name("app.open")
            .build();
        header_bar.pack_start(&open_button);
        let new_button = gtk::Button::builder()
            .tooltip_text("New file")
            .image(&tool_icon("document-new-symbolic"))
            .action_name("app.new")
            .build();
        header_bar.pack_start(&new_button);

        let menu = gtk::MenuButton::builder()
            .image(&tool_icon("open-menu-symbolic"))
            .menu_model(&crate::ui::menu::create_main_menu())
            .build();
        header_bar.pack_end(&menu);

        let save_box = linked_button_box();
        let save_button = gtk::Button::builder()
            .label("Save")
            .action_name("file.save")
            .build();
        save_box.pack_start(&save_button, false, false, 0);
        let save_as_button = gtk::Button::builder()
            .tooltip_text("Save file as...")
            .image(&tool_icon("document-save-as-symbolic"))
            .action_name("file.save-as")
            .build();
        save_box.pack_start(&save_as_button, false, false, 0);
        header_bar.pack_end(&save_box);

        let data = PSStore::new();
        let view = PSTreeView::new();
        let preview = PSPreviewPanel::new();
        let toast = Toast::new();

        let dashboard = PSDashboard::new();

        let grid = gtk::Grid::new();

        let merge_bar = create_merge_bar();
        grid.attach(&merge_bar, 0, 1, 1, 1);

        let (search_bar, search_entry) = create_search_bar();
        grid.attach(&search_bar, 0, 2, 1, 1);

        let tree_container = gtk::Grid::new();
        tree_container.attach(&scrolled(&view.get_widget()), 0, 0, 1, 1);
        let tree_action_bar = gtk::ActionBar::builder().hexpand(true).build();
        tree_action_bar.pack_start(&action_menu_button(
            &create_add_entity_menu(),
            "list-add-symbolic",
            "Add new record",
        ));
        tree_action_bar.pack_start(&action_button(
            "entry.delete",
            "list-remove-symbolic",
            "Remove record",
        ));
        tree_action_bar.pack_start(&action_button(
            "entry.edit",
            "document-edit-symbolic",
            "Edit record",
        ));
        tree_action_bar.pack_end(&action_button(
            "entry.copy-password",
            "dialog-password-symbolic",
            "Copy password",
        ));
        tree_action_bar.pack_end(&action_button(
            "entry.copy-name",
            "edit-copy-symbolic",
            "Copy name",
        ));
        tree_container.attach(&tree_action_bar, 0, 1, 1, 1);

        let stack = gtk::Stack::new()
            .named("dashboard", &dashboard.get_widget())
            .named(
                "file",
                &overlayed(
                    &paned(&tree_container, &preview.get_widget()),
                    &toast.as_widget(),
                ),
            );
        grid.attach(&stack, 0, 3, 1, 1);

        win.add(&grid);

        view.set_model(&data.as_model());

        let doc_actions = gio::SimpleActionGroup::new();
        win.register_doc_actions(&doc_actions);
        win.insert_action_group("doc", Some(&doc_actions));

        let file_actions = gio::SimpleActionGroup::new();
        win.register_file_actions(&file_actions);
        win.insert_action_group("file", Some(&file_actions));

        let merge_actions = gio::SimpleActionGroup::new();
        win.register_merge_actions(&merge_actions);
        win.insert_action_group("merge", Some(&merge_actions));

        let entry_actions = gio::SimpleActionGroup::new();
        win.register_entry_actions(&entry_actions);
        win.insert_action_group("entry", Some(&entry_actions));

        let private = PSMainWindowPrivate {
            mode: Cell::new(AppMode::Initial),
            header_bar,
            stack,
            dashboard,
            data: RefCell::new(data),
            view,
            search_entry,
            search_bar,
            preview,
            merge_bar,
            toast,

            doc_actions,
            file_actions,
            merge_actions,
            entry_actions,

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

        let delete_handler = win.connect_delete_event(
            clone!(@weak win => @default-return gtk::Inhibit(false), move |_win, _event| {
                glib::MainContext::default().spawn_local(async move {
                    win.on_close().await;
                });
                gtk::Inhibit(true)
            }),
        );
        *self.delete_handler.borrow_mut() = Some(delete_handler);

        win.private()
            .search_entry
            .connect_search_changed(clone!(@weak win => move |entry| {
                win.search(&entry.text(), SearchEvent::Change);
            }));
        win.private()
            .search_entry
            .connect_next_match(clone!(@weak win => move |entry| {
                win.search(&entry.text(), SearchEvent::Next);
            }));
        win.private()
            .search_entry
            .connect_previous_match(clone!(@weak win => move |entry| {
                win.search(&entry.text(), SearchEvent::Prev);
            }));

        win.private()
            .view
            .connect_cursor_changed(clone!(@weak win => move |selection| {
                let record = selection.and_then(|(iter, _path)| {
                    win.private().data.borrow().get(&iter)
                });
                win.listview_cursor_changed(record);
            }));

        win.private()
            .view
            .connect_drop(clone!(@weak win => @default-return false, move |iter| {
                let record_opt = win.private().data.borrow().get(&iter);
                record_opt.map_or(false, |record| record.record_type.is_group)
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

    fn set_changed(&self, changed: bool) {
        self.private().changed.set(changed);
        self.update_title();
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

    fn get_selected_group_iter(&self) -> Option<gtk::TreeIter> {
        let (iter, _path) = self.private().view.get_selected_iter()?;
        let model = &self.private().data.borrow();
        for (i, record) in model.parents(&iter) {
            if record.record_type.is_group {
                return Some(i);
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

        self.private().preview.update_record(record);
    }

    fn search(&self, search_text: &str, event: SearchEvent) {
        guard!(let Some(search_text) = search_text.non_empty() else { return });

        let private = self.private();
        let model = private.data.borrow().as_model();
        let iters = flatten_tree(&model);

        let mut search_iter: Box<dyn Iterator<Item = &gtk::TreeIter>> = match event {
            SearchEvent::Change | SearchEvent::Next => Box::new(iters.iter()),
            SearchEvent::Prev => Box::new(iters.iter().rev()),
        };
        if let Some((_selection_iter, selection_path)) = private.view.get_selected_iter() {
            search_iter = Box::new(
                search_iter
                    .skip_while(move |iter| model.path(iter).as_ref() != Some(&selection_path)),
            );
        }
        match event {
            SearchEvent::Change => {}
            SearchEvent::Next | SearchEvent::Prev => search_iter = Box::new(search_iter.skip(1)),
        };

        let look_at_secrets = private.config.get().unwrap().get().search_in_secrets;

        let next_match = search_iter
            .filter_map(|iter| private.data.borrow().get(iter).map(|record| (iter, record)))
            .find(|(_iter, record)| record.has_text(search_text, look_at_secrets));

        if let Some(next_match) = next_match {
            private.view.select_iter(next_match.0);
            self.listview_cursor_changed(Some(next_match.1));
        } else {
            self.error_bell();
        }
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

    fn update_title(&self) {
        let private = self.private();
        let header_bar = &private.header_bar;
        match private.mode.get() {
            AppMode::Initial => {
                header_bar.set_subtitle(None);
                header_bar.set_has_subtitle(false);
            }
            _ => {
                let mut display_filename = private
                    .filename
                    .borrow()
                    .as_ref()
                    .map_or(String::from("Unnamed"), |filename| {
                        filename.display().to_string()
                    });

                if private.changed.get() {
                    display_filename += " \u{23FA}";
                }
                header_bar.set_subtitle(Some(&display_filename));
                header_bar.set_has_subtitle(true);
            }
        }
    }

    async fn save_data(&self, filename: &Path) -> Result<()> {
        if let Some(password) = self.ensure_password_is_set().await {
            let tree = self.private().data.borrow().to_tree();

            format::save_file(filename, &password, &tree)?;

            self.private()
                .toast
                .notify(&format!("File '{}' was saved", filename.display()));
            *self.private().filename.borrow_mut() = Some(filename.to_owned());
            self.set_changed(false);
            self.private().cache.get().unwrap().add_file(filename);
        }
        Ok(())
    }

    pub async fn new_file(&self) {
        if self.ensure_data_is_saved().await {
            let data = PSStore::new();
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            *self.private().filename.borrow_mut() = None;
            *self.private().password.borrow_mut() = None;
            self.private().search_bar.set_search_mode(false);

            self.set_mode(AppMode::FileOpened);
            self.set_changed(false);
            self.listview_cursor_changed(None);
        }
    }

    pub async fn do_open_file(&self, filename: &Path) {
        if let Some((entries, password)) =
            load_data(filename.to_owned(), &self.clone().upcast()).await
        {
            self.private().cache.get().unwrap().add_file(filename);
            self.private().search_bar.set_search_mode(false);

            let data = PSStore::from_tree(&entries);
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            *self.private().filename.borrow_mut() = Some(filename.to_owned());
            *self.private().password.borrow_mut() = Some(password);

            self.set_mode(AppMode::FileOpened);
            self.set_changed(false);
            self.listview_cursor_changed(None);
            self.private().view.get_widget().grab_focus();
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

                private.merge_bar.hide();
                private.view.set_selection_mode(false);
                private.stack.set_visible_child_name("dashboard");
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

                private.merge_bar.hide();
                private.view.set_selection_mode(false);
                private.stack.set_visible_child_name("file");
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

                private.merge_bar.show();
                private.view.set_selection_mode(true);
                private.stack.set_visible_child_name("file");
            }
        }
        private.search_bar.set_search_mode(false);
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

    pub fn new(app: &gtk::Application, config: &Rc<ConfigService>, cache: &Cache) -> Self {
        let win: Self = glib::Object::new(&[("application", app)]).expect("MainWindow is created");

        win.private().config.set(config.clone()).ok().unwrap();
        win.private().cache.set(cache.clone()).ok().unwrap();
        win.private().dashboard.update(cache);

        let show_secrets_on_preview = config.get().show_secrets_on_preview;
        win.private().preview.update_config(show_secrets_on_preview);
        config.subscribe(glib::clone!(@weak win => move |new_config| {
            win.private().preview.update_config(new_config.show_secrets_on_preview);
        }));

        win.show_all();
        win.set_mode(AppMode::Initial);
        if let Some(screen) = win.screen() {
            crate::css::load_css(&screen);
        }
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
            self.private().search_entry.set_text("");

            let data = PSStore::new();
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            *self.private().filename.borrow_mut() = None;
            *self.private().password.borrow_mut() = None;

            self.set_mode(AppMode::Initial);
            self.set_changed(false);
            self.listview_cursor_changed(None);
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

                self.set_changed(true);
            }
        }
    }

    #[action(name = "change-password")]
    async fn action_change_password(&self) {
        if let Some(new_password) = change_password(&self.clone().upcast()).await {
            *self.private().password.borrow_mut() = Some(new_password);
            self.set_changed(true);
        }
    }

    #[action(name = "find")]
    fn action_find(&self) {
        self.private().search_bar.set_search_mode(true);
        self.private().search_entry.grab_focus();
    }

    #[action(name = "add")]
    async fn action_add_record(&self, record_type_name: String) {
        let record_type = RecordType::find(&record_type_name).unwrap_or(&*RECORD_TYPE_GENERIC);

        let empty_record = record_type.new_record();
        if let Some(new_record) = edit_record(
            &empty_record,
            &self.clone().upcast(),
            "Add record",
            self.get_usernames(),
        )
        .await
        {
            let group_iter = self.get_selected_group_iter();
            let iter = self
                .private()
                .data
                .borrow()
                .append(group_iter.as_ref(), &new_record);
            self.private().view.select_iter(&iter);
            self.set_changed(true);
        }
    }
}

#[awesome_glib::actions(register_fn = "register_merge_actions")]
impl PSMainWindow {
    #[action(name = "uncheck-all")]
    fn action_uncheck_all(&self) {
        self.private().data.borrow().uncheck_all();
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
        self.private().view.select_iter(&iter);
        self.set_changed(true);
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
                    self.private().toast.notify("Name is copied to clipboard");
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
                    self.private()
                        .toast
                        .notify("Secret (password) is copied to clipboard");
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
                    "Edit record",
                    self.get_usernames(),
                )
                .await
                {
                    self.private().data.borrow().update(&iter, &new_record);
                    self.listview_cursor_changed(Some(new_record));
                    self.set_changed(true);
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
            self.set_changed(true);
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
        self.set_changed(true);
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
