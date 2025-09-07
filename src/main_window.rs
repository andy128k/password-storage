use crate::cache::Cache;
use crate::config::ConfigService;
use crate::format;
use crate::model::record::{RECORD_TYPE_GENERIC, Record, RecordType};
use crate::model::tree::{RecordNode, RecordTree};
use crate::ui;
use crate::ui::dashboard::PSDashboard;
use crate::ui::dialogs::ask_save::{AskSave, ask_save};
use crate::ui::dialogs::change_password::change_password;
use crate::ui::dialogs::file_chooser;
use crate::ui::dialogs::say::say;
use crate::ui::edit_record::dialog::edit_record;
use crate::ui::forms::entry::form_password_entry;
use crate::ui::open_file::OpenFile;
use crate::utils::typed_list_store::TypedListStore;
use crate::utils::ui::*;
use gtk::{gio, glib, prelude::*, subclass::prelude::*};
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
    pub filename: Option<PathBuf>,
    pub password: Option<String>,
    pub changed: bool,
}

mod imp {
    use super::*;
    use crate::ui::file_pane::FilePane;
    use crate::ui::toast::Toast;
    use std::cell::OnceCell;

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

        pub toast: Toast,
        pub dashboard: PSDashboard,
        pub open_file: OpenFile,
        pub file_pane: FilePane,

        pub delete_handler: RefCell<Option<glib::signal::SignalHandlerId>>,

        pub mode: Cell<AppMode>,
        pub file: RefCell<OpenedFile>,

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

            self.stack.set_hexpand(true);
            self.stack.set_vexpand(true);
            self.stack
                .set_transition_type(gtk::StackTransitionType::SlideLeftRight);
            self.stack
                .add_named(&self.dashboard.get_widget(), Some("dashboard"));
            self.stack.add_named(&self.open_file, Some("open_file"));
            self.stack.add_named(&self.file_pane, Some("file"));

            let main_pane = gtk::Grid::builder().build();
            main_pane.attach(&self.stack, 0, 0, 1, 1);

            let overlay = overlayed(&main_pane, &self.toast.as_widget());
            win.set_child(Some(&overlay));

            win.register_file_actions(&self.file_actions);
            win.insert_action_group("file", Some(&self.file_actions));

            let delete_handler = win.connect_close_request(move |win| {
                let win = win.clone();
                glib::spawn_future_local(async move {
                    win.on_close().await;
                });
                glib::Propagation::Stop
            });
            *self.delete_handler.borrow_mut() = Some(delete_handler);

            self.file_pane.connect_edit_record(glib::clone!(
                #[weak]
                win,
                #[upgrade_or]
                Box::pin(async move { None }),
                move |record_node| Box::pin(async move { win.action_edit(record_node).await })
            ));
            self.file_pane.connect_file_changed(glib::clone!(
                #[weak]
                win,
                move |_| {
                    win.set_changed(true);
                }
            ));
            self.file_pane.connect_user_notification(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_, message| {
                    imp.toast.notify(message);
                }
            ));
        }
    }

    impl WidgetImpl for PSMainWindow {}
    impl WindowImpl for PSMainWindow {}
    impl ApplicationWindowImpl for PSMainWindow {}

    impl PSMainWindow {
        pub fn set_mode(&self, mode: AppMode) {
            match mode {
                AppMode::Initial => {
                    self.file_actions.set_enabled(false);

                    self.stack.set_visible_child_name("dashboard");
                    if let Some(cache) = self.cache.get() {
                        self.dashboard.update(cache);
                    }
                    self.search_reset();
                }
                AppMode::FileOpened => {
                    self.file_actions.set_enabled(true);

                    self.stack.set_visible_child_name("file");
                    self.search_reset();
                }
            }
            self.mode.set(mode);
        }

        pub fn search_reset(&self) {
            self.file_pane.view().search_reset();
        }
    }
}

glib::wrapper! {
    pub struct PSMainWindow(ObjectSubclass<imp::PSMainWindow>)
        @extends gtk::ApplicationWindow, gtk::Window, gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget, gtk::Root, gtk::Native, gtk::ShortcutManager, gio::ActionMap, gio::ActionGroup;
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

    fn get_usernames(&self) -> Vec<String> {
        get_usernames(&self.imp().file_pane.file())
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
        let save_result = format::save_file(filename, password, &self.imp().file_pane.file());
        if let Err(error) = save_result {
            say(self.upcast_ref(), &error.to_string()).await;
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
            self.imp().set_mode(imp::AppMode::FileOpened);

            *self.file_mut() = OpenedFile {
                filename: None,
                password: None,
                changed: false,
            };
            self.imp().file_pane.set_file(RecordTree::default()).await;
            self.imp().search_reset();

            self.update_title();
        }
    }

    async fn load_data(&self, filename: PathBuf) -> Option<(RecordTree, String)> {
        self.imp().stack.set_visible_child_name("open_file");

        let result = self
            .imp()
            .open_file
            .run(move |password| format::load_file(&filename, password))
            .await;

        if result.is_none() {
            self.imp().stack.set_visible_child_name("dashboard");
        }

        result
    }

    pub async fn do_open_file(&self, filename: &Path) {
        if let Some((data, password)) = self.load_data(filename.to_owned()).await {
            self.imp().cache.get().unwrap().add_file(filename);

            *self.file_mut() = OpenedFile {
                filename: Some(filename.to_owned()),
                password: Some(password),
                changed: false,
            };
            self.imp().file_pane.set_file(data).await;
            self.imp().search_reset();
            self.imp().set_mode(imp::AppMode::FileOpened);

            self.update_title();
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
        let Some(ref filename) = self.get_file_path().await else {
            return false;
        };
        let Some(ref password) = self.get_file_password().await else {
            return false;
        };
        self.save_data(filename, password).await
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

        // let config = config_service.get();
        // win.imp().search_bar.configure(config.search_in_secrets);
        // config_service.on_change.subscribe(glib::clone!(
        //     #[weak]
        //     win,
        //     move |new_config| {
        //         win.imp().search_bar.configure(new_config.search_in_secrets);
        //     }
        // ));

        win.present();
        win.imp().set_mode(imp::AppMode::Initial);
        crate::css::load_css(&RootExt::display(&win));
        win
    }
}

#[awesome_glib::actions(register_fn = "register_file_actions")]
impl PSMainWindow {
    #[action(name = "close")]
    async fn action_close_file(&self) {
        if self.ensure_data_is_saved().await {
            self.imp().set_mode(imp::AppMode::Initial);
            *self.file_mut() = OpenedFile {
                filename: None,
                password: None,
                changed: false,
            };
            self.imp().file_pane.set_file(RecordTree::default()).await;
            self.imp().search_reset();
            self.update_title();
        }
    }

    #[action(name = "save")]
    async fn action_save(&self) {
        let _saved = self.do_save().await;
    }

    #[action(name = "save-as")]
    async fn action_save_as(&self) {
        let Some(ref filename) = file_chooser::save_file(self.upcast_ref()).await else {
            return;
        };
        let Some(ref password) = self.get_file_password().await else {
            return;
        };
        let _saved = self.save_data(filename, password).await;
    }

    #[action(name = "merge-file")]
    async fn action_merge_file(&self) {
        let window = self.upcast_ref();
        let Some(filename) = file_chooser::open_file(window).await else {
            return;
        };
        let Some((extra_records, _password)) = self.load_data(filename).await else {
            return;
        };

        // TODO: maybe do merge into current folder?
        let merged_tree =
            crate::model::merge_trees::merge_trees(&self.imp().file_pane.file(), &extra_records);

        self.imp().file_pane.set_file(merged_tree).await;
        self.imp().search_reset();
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
        self.imp().file_pane.view().search_start();
    }

    #[action(name = "add")]
    async fn action_add_record(&self, record_type_name: String) {
        let record_type = RecordType::find(&record_type_name).unwrap_or(&RECORD_TYPE_GENERIC);

        let empty_record = record_type.new_record();
        let Some(new_record) = self.edit_record("Add record", &empty_record).await else {
            return;
        };

        let record_node = if new_record.record_type.is_group {
            RecordNode::group(new_record, &Default::default())
        } else {
            RecordNode::leaf(new_record)
        };
        self.imp().file_pane.append_record(&record_node).await;
        self.set_changed(true);
    }
}

impl PSMainWindow {
    async fn edit_record(&self, title: &str, record: &Record) -> Option<Record> {
        let result = edit_record(record, self.upcast_ref(), title, self.get_usernames()).await;
        self.imp().file_pane.grab_focus_to_view();
        pending().await;
        result
    }

    async fn action_edit(&self, record_node: RecordNode) -> Option<RecordNode> {
        let new_record = self
            .edit_record("Edit record", record_node.record())
            .await?;

        self.set_changed(true);
        Some(record_node.with_record(new_record))
    }
}

async fn new_password(parent_window: &gtk::Window) -> Option<String> {
    // TODO: ADD confirmation
    let mut form = ui::forms::form::Form::new();
    form.add("Password", Box::new(form_password_entry()), true);
    let result = ui::edit_object::edit_object(None, form, parent_window, "Enter password").await;
    result.map(|mut values| values.remove(0))
}

fn get_usernames(data: &RecordTree) -> Vec<String> {
    fn traverse(records: &TypedListStore<RecordNode>, usernames: &mut BTreeSet<String>) {
        for record in records {
            if let Some(username) = record.record().username()
                && !username.is_empty()
                && !usernames.contains(username)
            {
                usernames.insert(username.to_string());
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
