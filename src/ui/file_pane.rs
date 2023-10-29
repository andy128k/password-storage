use crate::model::record::Record;
use crate::model::tree::{RecordNode, RecordTree};
use crate::primary_accel;
use crate::ui::dialogs::say::say;
use crate::ui::group_selector::select_group;
use crate::ui::nav_bar::PSNavBar;
use crate::ui::record_view::view::PSRecordView;
use crate::utils::menu_builder::*;
use awesome_gtk::prelude::BitSetIterExt;
use gtk::{gio, glib, prelude::*, subclass::prelude::*};
use std::cell::Ref;

const ACTION_COPY_NAME: &str = "copy-name";
const ACTION_COPY_PASSWORD: &str = "copy-password";
const ACTION_MOVE: &str = "move";
const ACTION_EDIT: &str = "edit";
const ACTION_DELETE: &str = "delete";
const ACTION_MERGE: &str = "merge";

const SIGNAL_USER_NOTIFICATION: &str = "user-notification";
const SIGNAL_FILE_CHANGED: &str = "file-changed";
const SIGNAL_EDIT_RECORD: &str = "edit-record";

mod imp {
    use super::*;
    use crate::model::record::RECORD_TYPES;
    use crate::primary_accel;
    use crate::ui::record_type_popover::RecordTypePopoverBuilder;
    use crate::utils::typed_list_store::TypedListStore;
    use crate::utils::ui::{action_button, action_popover_button};
    use std::cell::RefCell;
    use std::sync::OnceLock;

    #[derive(Default)]
    pub struct FilePane {
        pub nav_bar: PSNavBar,
        pub view: PSRecordView,
        pub file: RefCell<RecordTree>,
        pub current_path: TypedListStore<RecordNode>,
        pub current_records: RefCell<TypedListStore<RecordNode>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for FilePane {
        const NAME: &'static str = "PSFilePane";
        type Type = super::FilePane;
        type ParentType = gtk::Widget;

        fn class_init(klass: &mut Self::Class) {
            klass.install_action(ACTION_COPY_NAME, None, |obj, _, _| {
                obj.action_copy_name();
            });
            klass.install_action(ACTION_COPY_PASSWORD, None, |obj, _, _| {
                obj.action_copy_password();
            });
            klass.install_action_async(ACTION_MOVE, None, |obj, _, _| async move {
                obj.action_move().await;
            });
            klass.install_action_async(ACTION_EDIT, None, |obj, _, _| async move {
                obj.action_edit().await;
            });
            klass.install_action_async(ACTION_DELETE, None, |obj, _, _| async move {
                obj.action_delele().await;
            });
            klass.install_action_async(ACTION_MERGE, None, |obj, _, _| async move {
                obj.action_merge().await;
            });
        }
    }

    impl ObjectImpl for FilePane {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            self.view.set_vexpand(true);
            self.view.set_popup(&create_popup());

            let action_bar = gtk::ActionBar::builder().hexpand(true).build();
            action_bar.pack_start(&action_popover_button(
                &RecordTypePopoverBuilder::default()
                    .record_types(&RECORD_TYPES)
                    .action_name_func(|record_type| format!("file.add::{}", record_type.name))
                    .build(),
                "ps-add",
                "Add new record",
            ));
            action_bar.pack_start(&action_button(ACTION_DELETE, "ps-remove", "Remove record"));
            action_bar.pack_start(&action_button(ACTION_EDIT, "ps-edit", "Edit record"));
            action_bar.pack_start(&action_button(
                ACTION_MERGE,
                "merge",
                "Merge selected records",
            ));
            action_bar.pack_end(&action_button(
                ACTION_COPY_PASSWORD,
                "dialog-password-symbolic",
                "Copy password",
            ));
            action_bar.pack_end(&action_button(
                ACTION_COPY_NAME,
                "edit-copy-symbolic",
                "Copy name",
            ));

            let grid = gtk::Grid::new();
            grid.attach(&self.nav_bar, 0, 0, 1, 1);
            grid.attach(&self.view, 0, 1, 1, 1);
            grid.attach(&action_bar, 0, 2, 1, 1);
            grid.set_parent(&*obj);

            self.view.connect_selection_changed(
                glib::clone!(@weak obj => move |selected_records| {
                    obj.selection_changed(selected_records);
                }),
            );

            self.view
                .connect_record_activated(glib::clone!(@weak self as imp => move |position| {
                    glib::MainContext::default().spawn_local(async move {
                        imp.row_activated(position).await;
                    });
                }));

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

            self.set_view_model(&self.file.borrow().records);

            self.nav_bar
                .set_model(self.current_path.untyped().upcast_ref());

            let shortcuts = gtk::ShortcutController::new();
            shortcuts.add_shortcut(
                gtk::Shortcut::builder()
                    .action(&gtk::NamedAction::new(ACTION_COPY_NAME))
                    .trigger(&gtk::ShortcutTrigger::parse_string(primary_accel!("c")).unwrap())
                    .build(),
            );
            shortcuts.add_shortcut(
                gtk::Shortcut::builder()
                    .action(&gtk::NamedAction::new(ACTION_COPY_PASSWORD))
                    .trigger(
                        &gtk::ShortcutTrigger::parse_string(primary_accel!("<Shift>c")).unwrap(),
                    )
                    .build(),
            );
            obj.add_controller(shortcuts);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    glib::subclass::Signal::builder(SIGNAL_USER_NOTIFICATION)
                        .param_types([glib::GString::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_FILE_CHANGED).build(),
                    glib::subclass::Signal::builder(SIGNAL_EDIT_RECORD)
                        .param_types([u32::static_type(), RecordNode::static_type()])
                        .build(),
                ]
            })
        }

        fn dispose(&self) {
            while let Some(child) = self.obj().first_child() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for FilePane {}

    impl FilePane {
        pub fn set_view_model(&self, model: &TypedListStore<RecordNode>) {
            *self.current_records.borrow_mut() = model.clone();
            self.view.set_model(model.untyped().upcast_ref());
        }

        async fn go_home(&self) {
            self.current_path.remove_all();
            self.set_view_model(&self.file.borrow().records);
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
                None => self.file.borrow().records.clone(),
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
                None => self.file.borrow().records.clone(),
            };
            self.set_view_model(&records);
            if let Some(prev) = prev {
                self.view.select_object(prev.upcast_ref()).await;
            }
        }

        async fn row_activated(&self, position: u32) {
            let Some(record) = self.current_records.borrow().get(position) else {
                return;
            };
            if let Some(children) = record.children() {
                self.current_path.append(&record);
                self.set_view_model(children);
                self.view.select_position_async(0).await;
            } else {
                self.obj().emit_edit_record(position, &record);
            }
        }
    }
}

glib::wrapper! {
    pub struct FilePane(ObjectSubclass<imp::FilePane>)
        @extends gtk::Widget;
}

impl Default for FilePane {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl FilePane {
    pub fn grab_focus_to_view(&self) {
        self.imp().view.grab_focus();
    }

    pub fn file(&self) -> Ref<'_, RecordTree> {
        self.imp().file.borrow()
    }

    pub async fn set_file(&self, file: RecordTree) {
        *self.imp().file.borrow_mut() = file;
        self.imp().current_path.remove_all();
        self.imp().set_view_model(&self.imp().file.borrow().records);

        self.imp().view.select_position_async(0).await;
        self.selection_changed(gtk::Bitset::new_empty());
        self.grab_focus_to_view();
    }

    fn get_record(&self, position: u32) -> Option<RecordNode> {
        self.imp().current_records.borrow().get(position)
    }

    pub fn append_record(&self, record_node: &RecordNode) {
        self.imp().current_records.borrow().append(record_node);
        let position = self.imp().current_records.borrow().len() - 1;
        self.imp().view.select_position(position);
        self.selection_changed(gtk::Bitset::new_range(position, 1));
    }

    pub fn replace_record(&self, position: u32, record_node: RecordNode) {
        self.imp()
            .current_records
            .borrow()
            .set(position, record_node);
        self.imp().view.select_position(position);
        self.selection_changed(gtk::Bitset::new_range(position, 1));
    }

    fn remove_record(&self, position: u32) {
        self.imp().current_records.borrow().remove(position);
        self.selection_changed(gtk::Bitset::new_empty());
    }

    fn remove_records(&self, mut positions: Vec<u32>) {
        positions.sort();
        for position in positions.into_iter().rev() {
            self.imp().current_records.borrow().remove(position);
        }
    }

    fn append_records_to(&self, parent: Option<&RecordNode>, records: &[RecordNode]) {
        match parent {
            None => {
                // move to root
                for record in records {
                    self.imp().file.borrow_mut().records.append(record);
                }
            }
            Some(group) => {
                // move to a group
                let children = group.children().expect("Only a group can be a destination");
                for record in records {
                    children.append(record);
                }
            }
        }
        self.selection_changed(gtk::Bitset::new_empty());
    }

    fn selection_changed(&self, selected: gtk::Bitset) {
        let selected_record = if selected.size() == 1 {
            self.get_record(selected.nth(0))
        } else {
            None
        };

        self.action_set_enabled(ACTION_COPY_NAME, selected_record.is_some());
        self.action_set_enabled(
            ACTION_COPY_PASSWORD,
            selected_record
                .as_ref()
                .and_then(|r| r.record().password())
                .is_some(),
        );
        self.action_set_enabled(ACTION_MOVE, selected.size() > 0);
        self.action_set_enabled(ACTION_EDIT, selected_record.is_some());
        self.action_set_enabled(ACTION_DELETE, selected_record.is_some());
        self.action_set_enabled(ACTION_MERGE, selected.size() > 1);
    }

    fn action_copy_name(&self) {
        let Some(position) = self.imp().view.get_selected_position() else {
            return;
        };
        let Some(record_node) = self.get_record(position) else {
            return;
        };
        let Some(username) = record_node.record().username() else {
            return;
        };
        self.clipboard().set_text(username);
        self.emit_user_notification("Name is copied to clipboard");
    }

    fn action_copy_password(&self) {
        let Some(position) = self.imp().view.get_selected_position() else {
            return;
        };
        let Some(record_node) = self.get_record(position) else {
            return;
        };
        let Some(password) = record_node.record().password() else {
            return;
        };
        self.clipboard().set_text(password);
        self.emit_user_notification("Secret (password) is copied to clipboard");
    }

    async fn action_move(&self) {
        let positions = self.imp().view.get_selected_positions();
        if positions.size() < 1 {
            return;
        }

        let Some(window) = self.root().and_downcast::<gtk::Window>() else {
            return;
        };
        let Some(dest) = select_group(&window, "Move to...", &self.file()).await else {
            return;
        };

        let (positions, records): (Vec<u32>, Vec<RecordNode>) = positions
            .iter_asc()
            .filter_map(|position| {
                let record = self.get_record(position)?;
                if dest.iter().any(|p| p == record) {
                    None
                } else {
                    Some((position, record))
                }
            })
            .unzip();

        self.remove_records(positions);
        self.append_records_to(dest.last().as_ref(), &records);
        self.emit_file_changed();
    }

    async fn action_edit(&self) {
        let Some(position) = self.imp().view.get_selected_position() else {
            return;
        };
        let Some(record_node) = self.get_record(position) else {
            return;
        };
        self.emit_edit_record(position, &record_node);
    }

    async fn action_delele(&self) {
        let Some(position) = self.imp().view.get_selected_position() else {
            return;
        };

        let Some(window) = self.root().and_downcast::<gtk::Window>() else {
            return;
        };
        let answer = gtk::AlertDialog::builder()
            .modal(true)
            .buttons(["Cancel", "Delete"])
            .default_button(0)
            .cancel_button(0)
            .message("Do you really want to delete selected entry?")
            .build()
            .choose_future(Some(&window))
            .await;
        if answer == Ok(1) {
            self.remove_record(position);
            self.emit_file_changed();
        }
    }

    async fn action_merge(&self) {
        let positions = self.imp().view.get_selected_positions();

        let (positions, records): (Vec<u32>, Vec<RecordNode>) = positions
            .iter_asc()
            .filter_map(|position| {
                let record = self.get_record(position)?;
                if record.is_group() {
                    None
                } else {
                    Some((position, record))
                }
            })
            .unzip();

        let Some(window) = self.root().and_downcast::<gtk::Window>() else {
            return;
        };
        if records.len() < 2 {
            say(&window, "Nothing to merge. Select few items and try again.").await;
            return;
        }

        {
            let mut message = String::from("Do you want to merge following items?\n");
            for record in &records {
                message.push('\n');
                message.push_str(&record.record().name());
            }

            let answer = gtk::AlertDialog::builder()
                .modal(true)
                .buttons(["Cancel", "Merge"])
                .default_button(1)
                .cancel_button(0)
                .message(&message)
                .build()
                .choose_future(Some(&window))
                .await;
            if answer != Ok(1) {
                return;
            }
        }

        self.remove_records(positions);

        // create new entry
        let result_node = {
            let r: Vec<Record> = records
                .into_iter()
                .map(|record_node| record_node.record().clone())
                .collect();
            let result = Record::join_entries(&r);
            RecordNode::leaf(result)
        };

        self.append_record(&result_node);
        self.emit_file_changed();
    }

    fn emit_user_notification(&self, message: &str) {
        self.emit_by_name::<()>(SIGNAL_USER_NOTIFICATION, &[&message]);
    }

    pub fn connect_user_notification<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self, &str) + 'static,
    {
        self.connect_closure(
            SIGNAL_USER_NOTIFICATION,
            false,
            glib::closure_local!(move |self_: &Self, message: glib::GString| (f)(self_, &message)),
        )
    }

    fn emit_file_changed(&self) {
        self.emit_by_name::<()>(SIGNAL_FILE_CHANGED, &[]);
    }

    pub fn connect_file_changed<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self) + 'static,
    {
        self.connect_closure(
            SIGNAL_FILE_CHANGED,
            false,
            glib::closure_local!(move |self_: &Self| (f)(self_)),
        )
    }

    fn emit_edit_record(&self, position: u32, record_node: &RecordNode) {
        self.emit_by_name::<()>(SIGNAL_EDIT_RECORD, &[&position, &record_node]);
    }

    pub fn connect_edit_record<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self, u32, RecordNode) + 'static,
    {
        self.connect_closure(
            SIGNAL_EDIT_RECORD,
            false,
            glib::closure_local!(move |self_: &Self, position, record_node| (f)(
                self_,
                position,
                record_node
            )),
        )
    }
}

fn create_popup() -> gio::MenuModel {
    gio::Menu::new()
        .section(
            gio::Menu::new()
                .item(
                    gio::MenuItem::create()
                        .action(ACTION_COPY_NAME)
                        .label("Copy _name")
                        .accel(primary_accel!("c")),
                )
                .item(
                    gio::MenuItem::create()
                        .action(ACTION_COPY_PASSWORD)
                        .label("Copy pass_word")
                        .accel(primary_accel!("<Shift>c")),
                ),
        )
        .section(
            gio::Menu::new().item(
                gio::MenuItem::create()
                    .action(ACTION_MOVE)
                    .label("_Move to..."),
            ),
        )
        .section(
            gio::Menu::new()
                .item(gio::MenuItem::create().action(ACTION_EDIT).label("_Edit"))
                .item(
                    gio::MenuItem::create()
                        .action(ACTION_DELETE)
                        .label("_Delete"),
                ),
        )
        .upcast()
}
