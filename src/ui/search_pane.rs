use crate::model::tree::RecordNode;
use crate::search::item::SearchMatch;
use crate::utils::typed_list_store::TypedListStore;
use gtk::{glib, prelude::*, subclass::prelude::*};

const ACTION_COPY_NAME: &str = "copy-name";
const ACTION_COPY_PASSWORD: &str = "copy-password";
const ACTION_EDIT: &str = "edit";

const SIGNAL_GO_HOME: &str = "ps-search-go-home";
const SIGNAL_USER_NOTIFICATION: &str = "user-notification";
const SIGNAL_FILE_CHANGED: &str = "file-changed";
const SIGNAL_EDIT_RECORD: &str = "edit-record";

mod imp {
    use super::*;
    use crate::primary_accel;
    use crate::ui::record_context_menu::record_context_menu;
    use crate::ui::record_view::has_record::{PSRecordViewOptions, SEARCH_MATCH_HAS_RECORD};
    use crate::ui::record_view::view::PSRecordView;
    use crate::utils::typed_list_store::TypedListStore;
    use crate::utils::ui::action_button;
    use std::cell::RefCell;
    use std::sync::OnceLock;

    pub struct SearchPane {
        pub view: PSRecordView,
        pub current_records: RefCell<TypedListStore<SearchMatch>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for SearchPane {
        const NAME: &'static str = "PSSearchPane";
        type Type = super::SearchPane;
        type ParentType = gtk::Widget;

        fn class_init(klass: &mut Self::Class) {
            klass.install_action(ACTION_COPY_NAME, None, |obj, _, _| {
                obj.action_copy_name();
            });
            klass.install_action(ACTION_COPY_PASSWORD, None, |obj, _, _| {
                obj.action_copy_password();
            });
            klass.install_action_async(ACTION_EDIT, None, |obj, _, _| async move {
                obj.action_edit().await;
            });
        }

        fn new() -> Self {
            Self {
                view: PSRecordView::new(PSRecordViewOptions {
                    has_record: SEARCH_MATCH_HAS_RECORD,
                    drag_and_drop: false,
                }),
                current_records: Default::default(),
            }
        }
    }

    impl ObjectImpl for SearchPane {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            self.view.set_vexpand(true);
            self.view.set_popup(&record_context_menu());

            let action_bar = gtk::ActionBar::builder().hexpand(true).build();
            action_bar.pack_start(&action_button(ACTION_EDIT, "ps-edit", "Edit record"));
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
            grid.attach(&self.view, 0, 0, 1, 1);
            grid.attach(&action_bar, 0, 1, 1, 1);
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

            self.view
                .connect_go_home(glib::clone!(@weak obj => move || obj.emit_go_home()));

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
                    glib::subclass::Signal::builder(SIGNAL_GO_HOME).build(),
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

    impl WidgetImpl for SearchPane {}

    impl SearchPane {
        pub fn set_view_model(&self, model: &TypedListStore<SearchMatch>) {
            *self.current_records.borrow_mut() = model.clone();
            self.view.set_model(model.untyped().upcast_ref());
        }

        async fn row_activated(&self, position: u32) {
            let Some(search_match) = self.current_records.borrow().get(position) else {
                return;
            };
            self.obj().emit_edit_record(position, search_match.record());
        }
    }
}

glib::wrapper! {
    pub struct SearchPane(ObjectSubclass<imp::SearchPane>)
        @extends gtk::Widget;
}

impl Default for SearchPane {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl SearchPane {
    pub fn grab_focus_to_view(&self) {
        self.imp().view.grab_focus();
    }

    pub async fn set_model(&self, model: TypedListStore<SearchMatch>) {
        self.imp().set_view_model(&model);
    }

    pub async fn reset(&self) {
        self.set_model(Default::default()).await;
    }

    fn get_search_match(&self, position: u32) -> Option<SearchMatch> {
        self.imp().current_records.borrow().get(position)
    }

    fn get_record(&self, position: u32) -> Option<RecordNode> {
        let search_match = self.get_search_match(position)?;
        Some(search_match.record().clone())
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
        self.action_set_enabled(ACTION_EDIT, selected_record.is_some());
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

    async fn action_edit(&self) {
        let Some(position) = self.imp().view.get_selected_position() else {
            return;
        };
        let Some(search_match) = self.get_search_match(position) else {
            return;
        };
        self.emit_edit_record(position, search_match.record());
    }
}

impl SearchPane {
    fn emit_go_home(&self) {
        self.emit_by_name::<()>(SIGNAL_GO_HOME, &[]);
    }

    pub fn connect_go_home<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self) + 'static,
    {
        self.connect_closure(
            SIGNAL_GO_HOME,
            false,
            glib::closure_local!(move |self_: &Self| (f)(self_)),
        )
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
