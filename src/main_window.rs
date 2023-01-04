use crate::cache::Cache;
use crate::config::ConfigService;
use crate::error::*;
use crate::format;
use crate::gtk_prelude::*;
use crate::model::record::{Record, RecordType, FIELD_NAME, RECORD_TYPES, RECORD_TYPE_GENERIC};
use crate::model::tree::RecordTree;
use crate::store::{PSStore, TreeTraverseEvent};
use crate::ui;
use crate::ui::dashboard::PSDashboard;
use crate::ui::dialogs::ask::{confirm_likely, confirm_unlikely};
use crate::ui::dialogs::ask_save::{ask_save, AskSave};
use crate::ui::dialogs::change_password::change_password;
use crate::ui::dialogs::file_chooser::{open_file, save_file};
use crate::ui::dialogs::read_file::read_file;
use crate::ui::dialogs::say::{say_error, say_info};
use crate::ui::edit_record::edit_record;
use crate::ui::record_type_popover::RecordTypePopoverBuilder;
use crate::ui::search::{PSSearchBar, SearchEvent, SearchEventType};
use crate::ui::toast::Toast;
use crate::ui::tree_view::PSTreeView;
use crate::utils::tree::flatten_tree;
use crate::utils::ui::*;
use once_cell::unsync::OnceCell;
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;
use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::rc::Rc;

const WINDOW_TITLE: &str = "Password Storage";

#[derive(Clone, Copy)]
enum AppMode {
    Initial,
    FileOpened,
}

struct PSMainWindowPrivate {
    mode: Cell<AppMode>,
    header_bar: gtk::HeaderBar,
    stack: gtk::Stack,
    dashboard: PSDashboard,
    data: RefCell<PSStore>,
    view: PSTreeView,

    file_actions: gio::SimpleActionGroup,
    entry_actions: gio::SimpleActionGroup,

    search_bar: PSSearchBar,
    toast: Toast,

    filename: RefCell<Option<PathBuf>>,
    password: RefCell<Option<String>>,
    changed: Cell<bool>,

    config_service: OnceCell<Rc<ConfigService>>,
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
    fn constructed(&self) {
        self.parent_constructed();

        let win = self.obj();

        win.set_icon_name(Some("password-storage"));

        win.set_default_width(1000);
        win.set_default_height(800);

        let header_bar = gtk::HeaderBar::builder()
            .show_title_buttons(true)
            .title_widget(&crate::utils::ui::title(WINDOW_TITLE))
            .build();
        win.set_titlebar(Some(&header_bar));

        let open_button = gtk::Button::builder()
            .label("Open")
            .action_name("app.open")
            .build();
        header_bar.pack_start(&open_button);
        let new_button = gtk::Button::builder()
            .tooltip_text("New file")
            .icon_name("document-new-symbolic")
            .action_name("app.new")
            .build();
        header_bar.pack_start(&new_button);

        let menu = gtk::MenuButton::builder()
            .icon_name("open-menu-symbolic")
            .menu_model(&crate::ui::menu::create_main_menu())
            .build();
        header_bar.pack_end(&menu);

        let save_box = linked_button_box();
        let save_button = gtk::Button::builder()
            .label("Save")
            .action_name("file.save")
            .build();
        save_box.append(&save_button);
        let save_as_button = gtk::Button::builder()
            .tooltip_text("Save file as...")
            .icon_name("document-save-as-symbolic")
            .action_name("file.save-as")
            .build();
        save_box.append(&save_as_button);
        header_bar.pack_end(&save_box);

        let data = PSStore::new();
        let view = PSTreeView::default();
        let toast = Toast::new();

        let dashboard = PSDashboard::new();

        let grid = gtk::Grid::new();

        let search_bar = PSSearchBar::new();
        grid.attach(&search_bar.get_widget(), 0, 2, 1, 1);

        let tree_container = gtk::Grid::new();
        view.set_vexpand(true);
        tree_container.attach(&view, 0, 0, 1, 1);
        let tree_action_bar = gtk::ActionBar::builder().hexpand(true).build();
        tree_action_bar.pack_start(&action_popover_button(
            &RecordTypePopoverBuilder::default()
                .record_types(&*RECORD_TYPES)
                .action_name_func(|record_type| format!("file.add::{}", record_type.name))
                .build(),
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
        tree_action_bar.pack_start(&action_button(
            "file.merge",
            "merge",
            "Merge selected records",
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
            .named("file", &overlayed(&tree_container, &toast.as_widget()));
        grid.attach(&stack, 0, 3, 1, 1);

        win.set_child(Some(&grid));

        view.set_model(Some(&data.as_model()));

        let file_actions = gio::SimpleActionGroup::new();
        win.register_file_actions(&file_actions);
        win.insert_action_group("file", Some(&file_actions));

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
            search_bar,
            toast,

            file_actions,
            entry_actions,

            filename: RefCell::new(None),
            password: RefCell::new(None),
            changed: Cell::new(false),

            config_service: OnceCell::new(),
            cache: OnceCell::new(),
        };

        self.private
            .set(private)
            .ok()
            .expect("private is set only once");

        let delete_handler = win.connect_close_request(move |win| {
            let win = win.clone();
            glib::MainContext::default().spawn_local(async move {
                win.on_close().await;
            });
            glib::signal::Inhibit(true)
        });

        *self.delete_handler.borrow_mut() = Some(delete_handler);

        win.private()
            .search_bar
            .on_search
            .subscribe(clone!(@weak win => move |event| {
                win.search(event);
            }));
        win.private()
            .search_bar
            .on_configure
            .subscribe(clone!(@weak win => move |search_config| {
                win.private().config_service.get().unwrap()
                    .update(|config| {
                        config.search_in_secrets = search_config.search_in_secrets;
                    });
            }));

        win.private()
            .view
            .connect_record_changed(clone!(@weak win => move |selected_records| {
                win.listview_cursor_changed(selected_records);
            }));

        win.private()
            .view
            .connect_record_activated(clone!(@weak win => move |_record, _iter| {
                glib::MainContext::default().spawn_local(async move {
                    win.action_edit().await;
                });
            }));

        win.private()
            .view
            .connect_url_clicked(clone!(@weak win => move |record| {
                glib::MainContext::default().spawn_local(async move {
                    if let Some(url) = record.url() {
                        gtk::show_uri(Some(&win), url, 0);
                    }
                });
            }));

        let popup = ui::menu::create_tree_popup();
        win.private().view.set_popup(&popup);
    }
}

impl WidgetImpl for PSMainWindowInner {}
impl WindowImpl for PSMainWindowInner {}
impl ApplicationWindowImpl for PSMainWindowInner {}

glib::wrapper! {
    pub struct PSMainWindow(ObjectSubclass<PSMainWindowInner>)
        @extends gtk::ApplicationWindow, gtk::Window, gtk::Widget, @implements gio::ActionMap;
}

impl PSMainWindow {
    pub fn from_window(window: &gtk::Window) -> Option<Self> {
        window.clone().downcast::<Self>().ok()
    }

    fn private(&self) -> &PSMainWindowPrivate {
        let private = self.imp();
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
        let (selected_record, iter) = self.private().view.get_selected_record()?;
        let model = &self.private().data.borrow();
        if selected_record.record_type.is_group {
            return Some(iter);
        }
        model.parent(&iter)
    }

    fn listview_cursor_changed(&self, records: &[Record]) {
        let entry_actions = &self.private().entry_actions;
        let is_selected_record = records.len() == 1;

        entry_actions
            .simple_action("copy-name")
            .set_enabled(is_selected_record);
        entry_actions
            .simple_action("copy-password")
            .set_enabled(is_selected_record && records[0].password().is_some());
        entry_actions
            .simple_action("edit")
            .set_enabled(is_selected_record);
        entry_actions
            .simple_action("delete")
            .set_enabled(is_selected_record);
        entry_actions
            .simple_action("convert-to")
            .set_enabled(is_selected_record && !records[0].record_type.is_group);

        self.private()
            .file_actions
            .simple_action("merge")
            .set_enabled(records.len() > 1);
    }

    fn search(&self, event: &SearchEvent) {
        if event.query.is_empty() {
            return;
        }

        let private = self.private();
        let model = private.data.borrow().as_model();
        let iters = flatten_tree(&model);

        let mut search_iter: Box<dyn Iterator<Item = &gtk::TreeIter>> = match event.event_type {
            SearchEventType::Change | SearchEventType::Next => Box::new(iters.iter()),
            SearchEventType::Prev => Box::new(iters.iter().rev()),
        };
        if let Some((_selection_record, selection_iter)) = private.view.get_selected_record() {
            search_iter = Box::new(
                search_iter.skip_while(move |iter| model.path(iter) != model.path(&selection_iter)),
            );
        }
        match event.event_type {
            SearchEventType::Change => {}
            SearchEventType::Next | SearchEventType::Prev => {
                search_iter = Box::new(search_iter.skip(1))
            }
        };

        let next_match = search_iter
            .map(|iter| (iter, private.data.borrow().get(iter)))
            .find(|(_iter, record)| record.has_text(&event.query, event.search_in_secrets));

        if let Some(next_match) = next_match {
            private.view.select_iter(next_match.0);
            self.listview_cursor_changed(&[next_match.1]);
        } else {
            self.error_bell();
        }
    }

    fn get_usernames(&self) -> Vec<String> {
        let mut result = BTreeSet::new();
        self.private().data.borrow().traverse_all(&mut |event| {
            let TreeTraverseEvent::Start { record, .. } = event else { return; };
            let Some(username) = record.username() else { return; };
            if !username.is_empty() && !result.contains(username) {
                result.insert(username.to_string());
            }
        });
        result.into_iter().collect()
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
                header_bar.set_title_widget(Some(&crate::utils::ui::title(WINDOW_TITLE)));
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

                header_bar.set_title_widget(Some(&crate::utils::ui::title_and_subtitle(
                    WINDOW_TITLE,
                    &display_filename,
                )));
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
            self.private().view.set_model(Some(&data.as_model()));

            *self.private().filename.borrow_mut() = None;
            *self.private().password.borrow_mut() = None;
            self.private().search_bar.set_search_mode(false);

            self.set_mode(AppMode::FileOpened);
            self.set_changed(false);
            self.listview_cursor_changed(&[]);
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
            self.private().view.set_model(Some(&data.as_model()));

            *self.private().filename.borrow_mut() = Some(filename.to_owned());
            *self.private().password.borrow_mut() = Some(password);

            self.set_mode(AppMode::FileOpened);
            self.set_changed(false);
            self.listview_cursor_changed(&[]);
            self.private().view.grab_focus();
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

    fn set_mode(&self, mode: AppMode) {
        let private = self.private();
        match mode {
            AppMode::Initial => {
                private.file_actions.set_enabled(false);
                private.entry_actions.set_enabled(false);

                private.stack.set_visible_child_name("dashboard");
                if let Some(cache) = self.private().cache.get() {
                    self.private().dashboard.update(cache);
                }
            }
            AppMode::FileOpened => {
                private.file_actions.set_enabled(true);
                private.entry_actions.set_enabled(true);

                private.stack.set_visible_child_name("file");
            }
        }
        private.search_bar.set_search_mode(false);
        self.private().mode.set(mode);
    }

    async fn on_close(&self) {
        if self.ensure_data_is_saved().await {
            self.clipboard().set_text(""); // clear

            if let Some(handler) = self.imp().delete_handler.take() {
                self.disconnect(handler);
            }
            self.close();
        }
    }

    pub fn new(app: &gtk::Application, config_service: &Rc<ConfigService>, cache: &Cache) -> Self {
        let win: Self = glib::Object::builder().property("application", app).build();

        win.private()
            .config_service
            .set(config_service.clone())
            .ok()
            .unwrap();
        win.private().cache.set(cache.clone()).ok().unwrap();
        win.private().dashboard.update(cache);

        let config = config_service.get();
        win.private().search_bar.configure(config.search_in_secrets);
        config_service
            .on_change
            .subscribe(glib::clone!(@weak win => move |new_config| {
                win.private().search_bar.configure(new_config.search_in_secrets);
            }));

        win.show();
        win.set_mode(AppMode::Initial);
        crate::css::load_css(&win.display());
        win
    }
}

#[awesome_glib::actions(register_fn = "register_file_actions")]
impl PSMainWindow {
    #[action(name = "close")]
    async fn action_close_file(&self) {
        if self.ensure_data_is_saved().await {
            self.private().search_bar.reset();

            let data = PSStore::new();
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(Some(&data.as_model()));

            *self.private().filename.borrow_mut() = None;
            *self.private().password.borrow_mut() = None;

            self.set_mode(AppMode::Initial);
            self.set_changed(false);
            self.listview_cursor_changed(&[]);
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
        self.private().search_bar.reset();

        let window = self.clone().upcast();
        if let Some(filename) = open_file(&window).await {
            if let Some((extra_records, _password)) = load_data(filename, &window).await {
                let records_tree = self.private().data.borrow().to_tree();
                let merged_tree =
                    crate::model::merge_trees::merge_trees(&records_tree, &extra_records);

                let data = PSStore::from_tree(&merged_tree);
                *self.private().data.borrow_mut() = data.clone();
                self.private().view.set_model(Some(&data.as_model()));

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

    #[action(name = "merge")]
    async fn action_merge(&self) {
        let (checked_records, checked_iters) = self.private().view.get_selected_records();

        if checked_records.len() < 2 {
            say_info(
                &self.clone().upcast(),
                "Nothing to merge. Select few items and try again.",
            )
            .await;
            return;
        }

        // rename (add path prefix to name)
        let records = checked_records;

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
        for iter in &checked_iters {
            self.private().data.borrow().delete(iter);
        }

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
        if let Some((record, _iter)) = self.private().view.get_selected_record() {
            if let Some(username) = record.username() {
                self.clipboard().set_text(username);
                self.private().toast.notify("Name is copied to clipboard");
            }
        }
    }

    #[action(name = "copy-password")]
    fn action_copy_password(&self) {
        if let Some((record, _iter)) = self.private().view.get_selected_record() {
            if let Some(password) = record.password() {
                self.clipboard().set_text(password);
                self.private()
                    .toast
                    .notify("Secret (password) is copied to clipboard");
            }
        }
    }

    #[action(name = "edit")]
    async fn action_edit(&self) {
        if let Some((record, iter)) = self.private().view.get_selected_record() {
            if let Some(new_record) = edit_record(
                &record,
                &self.clone().upcast(),
                "Edit record",
                self.get_usernames(),
            )
            .await
            {
                self.private().data.borrow().update(&iter, &new_record);
                self.listview_cursor_changed(&[new_record]);
                self.set_changed(true);
            }
        }
    }

    #[action(name = "delete")]
    async fn action_delele(&self) {
        let Some((_record, iter)) = self.private().view.get_selected_record() else { return };
        let confirmed = confirm_unlikely(
            &self.clone().upcast(),
            "Do you really want to delete selected entry?",
        )
        .await;
        if confirmed {
            self.private().data.borrow().delete(&iter);
            self.listview_cursor_changed(&[]);
            self.set_changed(true);
        }
    }

    #[action(name = "convert-to")]
    fn action_convert(&self, dest_record_type_name: String) {
        let Some((record, selection_iter)) = self.private().view.get_selected_record() else { return; };
        if record.record_type.is_group {
            return;
        }

        let Some(dest_record_type) = RecordType::find(&dest_record_type_name)
            .filter(|rt| !rt.is_group && !rt.ref_eq(record.record_type)) else { return; };

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
        self.listview_cursor_changed(&[new_record]);
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
        Box::new(ui::forms::entry::Text::new().for_password()),
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
