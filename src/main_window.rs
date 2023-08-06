use crate::cache::Cache;
use crate::config::ConfigService;
use crate::format;
use crate::model::record::{Record, RecordType, FIELD_NAME, RECORD_TYPES, RECORD_TYPE_GENERIC};
use crate::model::tree::RecordNode;
use crate::model::tree::RecordTree;
use crate::ui;
use crate::ui::dashboard::PSDashboard;
use crate::ui::dialogs::ask::{confirm_likely, confirm_unlikely};
use crate::ui::dialogs::ask_save::{ask_save, AskSave};
use crate::ui::dialogs::change_password::change_password;
use crate::ui::dialogs::file_chooser;
use crate::ui::dialogs::read_file::read_file;
use crate::ui::dialogs::say::{say_error, say_info};
use crate::ui::edit_record::edit_record;
use crate::ui::group_selector::select_group;
use crate::ui::record_type_popover::RecordTypePopoverBuilder;
use crate::ui::record_view::view::PSRecordView;
use crate::ui::search::{PSSearchBar, SearchEvent, SearchEventType};
use crate::ui::toast::Toast;
use crate::utils::typed_list_store::TypedListStore;
use crate::utils::ui::*;
use awesome_gtk::bitset::BitSetIterExt;
use gtk::{gio, glib, prelude::*, subclass::prelude::*};
use once_cell::unsync::OnceCell;
use std::cell::Ref;
use std::cell::RefMut;
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;
use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::rc::Rc;

const WINDOW_TITLE: &str = "Password Storage";

#[derive(Default)]
pub struct OpenedFile {
    pub data: RecordTree,
    pub filename: Option<PathBuf>,
    pub password: Option<String>,
    pub changed: bool,
}

mod imp {
    use super::*;
    use crate::ui::nav_bar::PSNavBar;

    #[derive(Clone, Copy, Default)]
    pub enum AppMode {
        #[default]
        Initial,
        FileOpened,
    }

    #[derive(Default)]
    pub struct PSMainWindow {
        pub header_bar: gtk::HeaderBar,
        pub stack: gtk::Stack,

        pub file_actions: gio::SimpleActionGroup,
        pub entry_actions: gio::SimpleActionGroup,

        pub dashboard: PSDashboard,

        pub toast: Toast,
        pub search_bar: PSSearchBar,
        pub nav_bar: PSNavBar,
        pub view: PSRecordView,

        pub delete_handler: RefCell<Option<glib::signal::SignalHandlerId>>,

        pub mode: Cell<AppMode>,
        pub file: RefCell<OpenedFile>,
        pub current_path: TypedListStore<RecordNode>,
        pub current_records: RefCell<TypedListStore<RecordNode>>,

        pub config_service: OnceCell<Rc<ConfigService>>,
        pub cache: OnceCell<Cache>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSMainWindow {
        const NAME: &'static str = "PSMainWindow";
        type Type = super::PSMainWindow;
        type ParentType = gtk::ApplicationWindow;
    }

    impl ObjectImpl for PSMainWindow {
        fn constructed(&self) {
            self.parent_constructed();

            let win = self.obj();

            win.set_icon_name(Some("password-storage"));

            win.set_default_width(1000);
            win.set_default_height(800);

            self.header_bar.set_show_title_buttons(true);
            self.header_bar
                .set_title_widget(Some(&crate::utils::ui::title(WINDOW_TITLE)));
            win.set_titlebar(Some(&self.header_bar));

            let open_button = gtk::Button::builder()
                .label("Open")
                .action_name("app.open")
                .build();
            self.header_bar.pack_start(&open_button);
            let new_button = gtk::Button::builder()
                .tooltip_text("New file")
                .icon_name("document-new-symbolic")
                .action_name("app.new")
                .build();
            self.header_bar.pack_start(&new_button);

            let menu = gtk::MenuButton::builder()
                .icon_name("open-menu-symbolic")
                .menu_model(&crate::ui::menu::create_main_menu())
                .primary(true)
                .build();
            self.header_bar.pack_end(&menu);

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
            self.header_bar.pack_end(&save_box);

            self.nav_bar
                .set_model(self.current_path.untyped().upcast_ref());
            self.nav_bar
                .connect_go_home(glib::clone!(@weak self as imp => move || {
                    glib::MainContext::default().spawn_local(async move {
                        imp.go_home().await
                    });
                }));
            self.nav_bar
                .connect_go_path(glib::clone!(@weak self as imp => move |position| {
                    glib::MainContext::default().spawn_local(async move {
                        imp.go_path(position).await
                    });
                }));
            self.nav_bar
                .connect_go_up(glib::clone!(@weak self as imp => move || {
                    glib::MainContext::default().spawn_local(async move {
                        imp.go_up().await
                    });
                }));

            let tree_container = gtk::Grid::new();
            self.view.set_vexpand(true);
            tree_container.attach(&self.search_bar, 0, 0, 3, 1);
            tree_container.attach(&self.nav_bar, 0, 1, 3, 1);
            tree_container.attach(&self.view, 0, 2, 3, 1);
            let tree_action_bar = gtk::ActionBar::builder().hexpand(true).build();
            tree_action_bar.pack_start(&action_popover_button(
                &RecordTypePopoverBuilder::default()
                    .record_types(&RECORD_TYPES)
                    .action_name_func(|record_type| format!("file.add::{}", record_type.name))
                    .build(),
                "ps-add",
                "Add new record",
            ));
            tree_action_bar.pack_start(&action_button(
                "entry.delete",
                "ps-remove",
                "Remove record",
            ));
            tree_action_bar.pack_start(&action_button("entry.edit", "ps-edit", "Edit record"));
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
            tree_container.attach(&tree_action_bar, 0, 3, 3, 1);

            self.stack
                .add_named(&self.dashboard.get_widget(), Some("dashboard"));
            self.stack.add_named(
                &overlayed(&tree_container, &self.toast.as_widget()),
                Some("file"),
            );
            win.set_child(Some(&self.stack));

            self.set_view_model(&self.file.borrow().data.records);

            win.register_file_actions(&self.file_actions);
            win.insert_action_group("file", Some(&self.file_actions));

            win.register_entry_actions(&self.entry_actions);
            win.insert_action_group("entry", Some(&self.entry_actions));

            let delete_handler = win.connect_close_request(move |win| {
                let win = win.clone();
                glib::MainContext::default().spawn_local(async move {
                    win.on_close().await;
                });
                glib::Propagation::Stop
            });
            *self.delete_handler.borrow_mut() = Some(delete_handler);

            self.search_bar
                .connect_search(glib::clone!(@weak win => move |event| {
                    win.search(&event);
                }));
            self.search_bar.connect_configure(
                glib::clone!(@weak self as imp => move |search_config| {
                    imp.config_service.get().unwrap()
                        .update(|config| {
                            config.search_in_secrets = search_config.search_in_secrets;
                        });
                }),
            );

            self.view.connect_selection_changed(
                glib::clone!(@weak win => move |selected_records| {
                    win.listview_cursor_changed(selected_records);
                }),
            );

            self.view
                .connect_record_activated(glib::clone!(@weak win => move |position| {
                    glib::MainContext::default().spawn_local(async move {
                        win.listview_row_activated(position).await;
                    });
                }));

            self.view
                .connect_go_home(glib::clone!(@weak self as imp => move || {
                    glib::MainContext::default().spawn_local(async move {
                        imp.go_home().await
                    });
                }));
            self.view
                .connect_go_up(glib::clone!(@weak self as imp => move || {
                    glib::MainContext::default().spawn_local(async move {
                        imp.go_up().await
                    });
                }));

            let popup = ui::menu::create_tree_popup();
            self.view.set_popup(&popup);
        }
    }

    impl WidgetImpl for PSMainWindow {}
    impl WindowImpl for PSMainWindow {}
    impl ApplicationWindowImpl for PSMainWindow {}

    impl PSMainWindow {
        pub fn set_view_model(&self, model: &TypedListStore<RecordNode>) {
            *self.current_records.borrow_mut() = model.clone();
            self.view.set_model(model.untyped().upcast_ref());
        }

        async fn go_home(&self) {
            self.current_path.remove_all();
            self.set_view_model(&self.file.borrow().data.records);
            self.view.select_position_async(0).await;
        }

        async fn go_path(&self, position: u32) {
            if position + 1 >= self.current_path.len() {
                return;
            }
            let prev = self.current_path.get(position + 1);
            self.current_path.truncate(position + 1);
            let records = match self.current_path.last() {
                Some(parent) => parent.children().unwrap().clone(),
                None => self.file.borrow().data.records.clone(),
            };
            self.set_view_model(&records);
            if let Some(prev) = prev {
                self.view.select_object(prev.upcast_ref()).await;
            }
        }

        async fn go_up(&self) {
            let prev = self.current_path.pop_back();
            let records = match self.current_path.last() {
                Some(parent) => parent.children().unwrap().clone(),
                None => self.file.borrow().data.records.clone(),
            };
            self.set_view_model(&records);
            if let Some(prev) = prev {
                self.view.select_object(prev.upcast_ref()).await;
            }
        }
    }
}

glib::wrapper! {
    pub struct PSMainWindow(ObjectSubclass<imp::PSMainWindow>)
        @extends gtk::ApplicationWindow, gtk::Window, gtk::Widget,
        @implements gio::ActionMap;
}

impl PSMainWindow {
    fn file(&self) -> Ref<'_, OpenedFile> {
        self.imp().file.borrow()
    }

    fn file_mut(&self) -> RefMut<'_, OpenedFile> {
        self.imp().file.borrow_mut()
    }

    fn set_changed(&self, changed: bool) {
        self.file_mut().changed = changed;
        self.update_title();
    }

    async fn ensure_data_is_saved(&self) -> bool {
        if !self.file().changed {
            return true;
        }
        let window = self.upcast_ref();
        match ask_save(
            window,
            "Save changes before closing? If you don't save, changes will be permanently lost.",
        )
        .await
        {
            AskSave::Save => self.do_save().await,
            AskSave::Discard => true,
            AskSave::Cancel => false,
        }
    }

    fn reset_records_view(&self) {
        self.imp().current_path.remove_all();
        self.imp().set_view_model(&self.file().data.records);
    }

    fn get_record(&self, position: u32) -> Option<RecordNode> {
        self.imp().current_records.borrow().get(position)
    }

    fn listview_cursor_changed(&self, selected: gtk::Bitset) {
        let entry_actions = &self.imp().entry_actions;
        let selected_record = if selected.size() == 1 {
            self.get_record(selected.nth(0))
        } else {
            None
        };

        entry_actions
            .simple_action("copy-name")
            .set_enabled(selected_record.is_some());
        entry_actions.simple_action("copy-password").set_enabled(
            selected_record
                .as_ref()
                .and_then(|r| r.record().password())
                .is_some(),
        );
        entry_actions
            .simple_action("edit")
            .set_enabled(selected_record.is_some());
        entry_actions
            .simple_action("delete")
            .set_enabled(selected_record.is_some());
        entry_actions
            .simple_action("convert-to")
            .set_enabled(selected_record.map_or(false, |r| !r.record().record_type.is_group));

        self.imp()
            .file_actions
            .simple_action("merge")
            .set_enabled(selected.size() > 1);
    }

    async fn listview_row_activated(&self, position: u32) {
        let Some(record) = self.get_record(position) else { return };
        if let Some(children) = record.children() {
            self.imp().current_path.append(&record);
            self.imp().set_view_model(children);
            self.imp().view.select_position_async(0).await;
        } else {
            self.action_edit().await;
        }
    }

    fn search(&self, event: &SearchEvent) {
        if event.query.is_empty() {
            return;
        }

        // let private = self.private();
        // let model = private.data.borrow().as_model();
        // let iters = flatten_tree(&model);

        // let mut search_iter: Box<dyn Iterator<Item = &gtk::TreeIter>> = match event.event_type {
        //     SearchEventType::Change | SearchEventType::Next => Box::new(iters.iter()),
        //     SearchEventType::Prev => Box::new(iters.iter().rev()),
        // };
        // if let Some((_selection_record, selection_iter)) = private.view.get_selected_record() {
        //     search_iter = Box::new(
        //         search_iter.skip_while(move |iter| model.path(iter) != model.path(&selection_iter)),
        //     );
        // }
        // match event.event_type {
        //     SearchEventType::Change => {}
        //     SearchEventType::Next | SearchEventType::Prev => {
        //         search_iter = Box::new(search_iter.skip(1))
        //     }
        // };

        // let next_match = search_iter
        //     .map(|iter| (iter, private.data.borrow().get(iter)))
        //     .find(|(_iter, record)| record.has_text(&event.query, event.search_in_secrets));

        // if let Some(next_match) = next_match {
        //     private.view.select_iter(next_match.0);
        //     self.listview_cursor_changed(&[next_match.1]);
        // } else {
        //     self.error_bell();
        // }
    }

    fn get_usernames(&self) -> Vec<String> {
        get_usernames(&self.imp().file.borrow().data)
    }

    async fn get_file_path(&self) -> Option<PathBuf> {
        if let Some(ref filename) = self.file().filename {
            return Some(filename.to_owned());
        }
        file_chooser::save_file(self.upcast_ref()).await
    }

    async fn get_file_password(&self) -> Option<String> {
        if let Some(ref password) = self.file().password {
            return Some(password.to_owned());
        }
        new_password(self.upcast_ref()).await
    }

    fn update_title(&self) {
        match self.imp().mode.get() {
            imp::AppMode::Initial => {
                self.imp()
                    .header_bar
                    .set_title_widget(Some(&crate::utils::ui::title(WINDOW_TITLE)));
            }
            imp::AppMode::FileOpened => {
                let mut display_filename = self
                    .file()
                    .filename
                    .as_ref()
                    .map_or(String::from("Unnamed"), |filename| {
                        filename.display().to_string()
                    });

                if self.file().changed {
                    display_filename += " \u{23FA}";
                }

                self.imp().header_bar.set_title_widget(Some(
                    &crate::utils::ui::title_and_subtitle(WINDOW_TITLE, &display_filename),
                ));
            }
        }
    }

    async fn save_data(&self, filename: &Path, password: &str) -> bool {
        let save_result = format::save_file(filename, password, &self.file().data);
        if let Err(error) = save_result {
            say_error(self.upcast_ref(), &error.to_string()).await;
            return false;
        }

        self.imp()
            .toast
            .notify(&format!("File '{}' was saved", filename.display()));
        self.file_mut().filename = Some(filename.to_owned());
        self.file_mut().password = Some(password.to_owned());
        self.set_changed(false);
        self.imp().cache.get().unwrap().add_file(filename);

        true
    }

    pub async fn new_file(&self) {
        if self.ensure_data_is_saved().await {
            *self.file_mut() = OpenedFile {
                data: RecordTree::default(),
                filename: None,
                password: None,
                changed: false,
            };

            self.set_mode(imp::AppMode::FileOpened);
            self.reset_records_view();

            self.update_title();
            self.imp().search_bar.set_search_mode(false);
            self.listview_cursor_changed(gtk::Bitset::new_empty());
        }
    }

    pub async fn do_open_file(&self, filename: &Path) {
        if let Some((data, password)) = load_data(filename.to_owned(), self.upcast_ref()).await {
            self.imp().cache.get().unwrap().add_file(filename);

            *self.file_mut() = OpenedFile {
                data,
                filename: Some(filename.to_owned()),
                password: Some(password),
                changed: false,
            };

            self.set_mode(imp::AppMode::FileOpened);
            self.reset_records_view();

            self.update_title();
            self.imp().search_bar.set_search_mode(false);
            self.imp().view.select_position_async(0).await;
            self.listview_cursor_changed(gtk::Bitset::new_empty());
            self.imp().view.grab_focus();
        }
    }

    pub async fn open_file(&self) {
        if self.ensure_data_is_saved().await {
            if let Some(filename) = file_chooser::open_file(self.upcast_ref()).await {
                self.do_open_file(&filename).await;
            }
        }
    }

    async fn do_save(&self) -> bool {
        let Some(ref filename) = self.get_file_path().await else { return false };
        let Some(ref password) = self.get_file_password().await else { return false };
        self.save_data(filename, password).await
    }

    fn set_mode(&self, mode: imp::AppMode) {
        match mode {
            imp::AppMode::Initial => {
                self.imp().file_actions.set_enabled(false);
                self.imp().entry_actions.set_enabled(false);

                self.imp().stack.set_visible_child_name("dashboard");
                if let Some(cache) = self.imp().cache.get() {
                    self.imp().dashboard.update(cache);
                }
            }
            imp::AppMode::FileOpened => {
                self.imp().file_actions.set_enabled(true);
                self.imp().entry_actions.set_enabled(true);

                self.imp().stack.set_visible_child_name("file");
            }
        }
        self.imp().search_bar.set_search_mode(false);
        self.imp().mode.set(mode);
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

        win.imp()
            .config_service
            .set(config_service.clone())
            .ok()
            .unwrap();
        win.imp().cache.set(cache.clone()).ok().unwrap();
        win.imp().dashboard.update(cache);

        let config = config_service.get();
        win.imp().search_bar.configure(config.search_in_secrets);
        config_service
            .on_change
            .subscribe(glib::clone!(@weak win => move |new_config| {
                win.imp().search_bar.configure(new_config.search_in_secrets);
            }));

        win.show();
        win.set_mode(imp::AppMode::Initial);
        crate::css::load_css(&win.display());
        win
    }
}

#[awesome_glib::actions(register_fn = "register_file_actions")]
impl PSMainWindow {
    #[action(name = "close")]
    async fn action_close_file(&self) {
        if self.ensure_data_is_saved().await {
            *self.file_mut() = OpenedFile {
                data: RecordTree::default(),
                filename: None,
                password: None,
                changed: false,
            };

            self.set_mode(imp::AppMode::Initial);
            self.reset_records_view();

            self.update_title();
            self.imp().search_bar.reset();
            self.listview_cursor_changed(gtk::Bitset::new_empty());
        }
    }

    #[action(name = "save")]
    async fn action_save(&self) {
        let _saved = self.do_save().await;
    }

    #[action(name = "save-as")]
    async fn action_save_as(&self) {
        let Some(ref filename) = file_chooser::save_file(self.upcast_ref()).await else { return };
        let Some(ref password) = self.get_file_password().await else { return };
        let _saved = self.save_data(filename, password).await;
    }

    #[action(name = "merge-file")]
    async fn action_merge_file(&self) {
        self.imp().search_bar.reset();

        let window = self.upcast_ref();
        let Some(filename) = file_chooser::open_file(window).await else { return };
        let Some((extra_records, _password)) = load_data(filename, window).await else { return };

        // TODO: maybe do merge into current folder?
        let merged_tree = crate::model::merge_trees::merge_trees(&self.file().data, &extra_records);

        self.file_mut().data = merged_tree;
        self.reset_records_view();
        self.imp().view.select_position_async(0).await;
        self.set_changed(true);
    }

    #[action(name = "change-password")]
    async fn action_change_password(&self) {
        if let Some(new_password) = change_password(self.upcast_ref()).await {
            self.file_mut().password = Some(new_password);
            self.set_changed(true);
        }
    }

    #[action(name = "find")]
    fn action_find(&self) {
        self.imp().search_bar.set_search_mode(true);
    }

    #[action(name = "add")]
    async fn action_add_record(&self, record_type_name: String) {
        let record_type = RecordType::find(&record_type_name).unwrap_or(&*RECORD_TYPE_GENERIC);

        let empty_record = record_type.new_record();
        let Some(new_record) = edit_record(
            &empty_record,
            self.upcast_ref(),
            "Add record",
            self.get_usernames(),
        ).await else { return };

        let record_node = if new_record.record_type.is_group {
            RecordNode::group(new_record, &Default::default())
        } else {
            RecordNode::leaf(new_record)
        };
        self.imp().current_records.borrow().append(&record_node);
        let position = self.imp().current_records.borrow().len() - 1;
        self.imp().view.select_position(position);
        self.listview_cursor_changed(gtk::Bitset::new_range(position, 1));
        self.set_changed(true);
    }

    #[action(name = "merge")]
    async fn action_merge(&self) {
        let positions = self.imp().view.get_selected_positions();

        let records: Vec<(u32, RecordNode)> = positions
            .iter_asc()
            .filter_map(|position| {
                let record = self.get_record(position)?;
                if record.is_group() {
                    None
                } else {
                    Some((position, record))
                }
            })
            .collect();

        if records.len() < 2 {
            say_info(
                self.upcast_ref(),
                "Nothing to merge. Select few items and try again.",
            )
            .await;
            return;
        }

        {
            let mut message = String::from("Do you want to merge following items?\n");
            for (_pos, record) in &records {
                message.push('\n');
                message.push_str(&record.record().name());
            }

            if !confirm_likely(self.upcast_ref(), &message).await {
                return;
            }
        }

        // delete entries
        for position in positions.iter_desc() {
            self.imp().current_records.borrow().remove(position);
        }

        // create new entry
        let result_node = {
            let r: Vec<Record> = records
                .into_iter()
                .map(|(_pos, record_node)| record_node.record().clone())
                .collect();
            let result = Record::join_entries(&r);
            RecordNode::leaf(result)
        };

        self.imp().current_records.borrow().append(&result_node);
        let position = self.imp().current_records.borrow().len() - 1;
        self.imp().view.select_position(position);
        self.listview_cursor_changed(gtk::Bitset::new_range(position, 1));
        self.set_changed(true);
    }
}

#[awesome_glib::actions(register_fn = "register_entry_actions")]
impl PSMainWindow {
    #[action(name = "copy-name")]
    fn action_copy_name(&self) {
        let Some(position) = self.imp().view.get_selected_position() else { return };
        let Some(record_node) = self.get_record(position) else { return };
        let Some(username) = record_node.record().username() else { return };
        self.clipboard().set_text(username);
        self.imp().toast.notify("Name is copied to clipboard");
    }

    #[action(name = "copy-password")]
    fn action_copy_password(&self) {
        let Some(position) = self.imp().view.get_selected_position() else { return };
        let Some(record_node) = self.get_record(position) else { return };
        let Some(password) = record_node.record().password() else { return };
        self.clipboard().set_text(password);
        self.imp()
            .toast
            .notify("Secret (password) is copied to clipboard");
    }

    #[action(name = "move")]
    async fn action_move(&self) {
        let positions = self.imp().view.get_selected_positions();
        if positions.size() < 1 {
            return;
        }

        let Some(dest) = select_group(
            self.upcast_ref(),
            "Move to...",
            &self.file().data,
        )
        .await else { return };

        let mut records = Vec::new();
        for position in positions.iter_desc() {
            let Some(record) = self.imp().current_records.borrow().get(position) else { continue };
            if dest.iter().any(|p| p == record) {
                continue;
            }
            self.imp().current_records.borrow().remove(position);
            records.push(record);
        }

        match dest.last() {
            None => {
                // move to root
                for record in records {
                    self.file().data.records.append(&record);
                }
            }
            Some(group) => {
                // move to a group
                let children = group.children().expect("Only a group can be a destination");
                for record in records {
                    children.append(&record);
                }
            }
        }

        self.listview_cursor_changed(gtk::Bitset::new_empty());
        self.set_changed(true);
    }

    #[action(name = "edit")]
    async fn action_edit(&self) {
        let Some(position) = self.imp().view.get_selected_position() else { return };
        let Some(record_node) = self.get_record(position) else { return };

        let Some(new_record) = edit_record(
            record_node.record(),
            self.upcast_ref(),
            "Edit record",
            self.get_usernames(),
        )
        .await else { return };

        self.imp()
            .current_records
            .borrow()
            .set(position, record_node.with_record(new_record));
        self.imp().view.select_position(position);
        self.listview_cursor_changed(gtk::Bitset::new_range(position, 1));
        self.set_changed(true);
    }

    #[action(name = "delete")]
    async fn action_delele(&self) {
        let Some(position) = self.imp().view.get_selected_position() else { return };

        let confirmed = confirm_unlikely(
            self.upcast_ref(),
            "Do you really want to delete selected entry?",
        )
        .await;
        if confirmed {
            self.imp().current_records.borrow().remove(position);
            self.listview_cursor_changed(gtk::Bitset::new_empty());
            self.set_changed(true);
        }
    }

    #[action(name = "convert-to")]
    fn action_convert(&self, dest_record_type_name: String) {
        let Some(position) = self.imp().view.get_selected_position() else { return };
        let Some(record_node) = self.get_record(position) else { return };
        if record_node.is_group() {
            return;
        }

        let Some(dest_record_type) = RecordType::find(&dest_record_type_name)
            .filter(|rt| !rt.is_group && *rt != record_node.record().record_type) else { return; };

        let new_record = {
            let mut new_record = dest_record_type.new_record();
            let name = record_node.record().get_field(&FIELD_NAME);
            new_record.set_field(&FIELD_NAME, name);
            new_record.join(record_node.record());
            new_record
        };
        self.imp()
            .current_records
            .borrow()
            .set(position, record_node.with_record(new_record));
        self.set_changed(true);
        self.imp().view.select_position(position);
        self.listview_cursor_changed(gtk::Bitset::new_range(position, 1));
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

fn get_usernames(data: &RecordTree) -> Vec<String> {
    fn traverse(records: &TypedListStore<RecordNode>, usernames: &mut BTreeSet<String>) {
        for record in records {
            if let Some(username) = record.record().username().filter(|u| !u.is_empty()) {
                if !usernames.contains(username) {
                    usernames.insert(username.to_string());
                }
            }
            if let Some(children) = record.children() {
                traverse(children, usernames);
            }
        }
    }

    let mut usernames = BTreeSet::new();
    traverse(&data.records, &mut usernames);
    usernames.into_iter().collect()
}
