use crate::gtk_prelude::*;
use crate::model::tree::RecordNode;
use crate::utils::typed_list_store::TypedListStore;
use crate::utils::ui::pending;
use std::cell::RefCell;
use std::rc::Rc;

mod imp {
    use super::*;
    use crate::ui::record_view::item::PSRecordViewItem;
    use crate::ui::record_view::item_factory::item_factory;
    use crate::utils::ui::{scrolled, PSWidgetExt};
    use crate::weak_map::WeakMap;
    use once_cell::sync::Lazy;

    pub const SIGNAL_GO_HOME: &str = "ps-go-home";
    pub const SIGNAL_GO_UP: &str = "ps-go-up";
    pub const SIGNAL_SELECTION_CHANGED: &str = "ps-selection-changed";
    pub const SIGNAL_RECORD_ACTIVATED: &str = "ps-record-activated";

    pub struct PSTreeView {
        pub list_view: gtk::ListView,
        pub selection: gtk::MultiSelection,
        pub popup_model: Rc<RefCell<Option<gio::MenuModel>>>,
        pub mapping: Rc<WeakMap<u32, PSRecordViewItem>>,
    }

    impl Default for PSTreeView {
        fn default() -> Self {
            Self {
                list_view: Default::default(),
                selection: gtk::MultiSelection::new(gio::ListModel::NONE),
                popup_model: Default::default(),
                mapping: Default::default(),
            }
        }
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

            self.list_view
                .set_factory(Some(&item_factory(self.popup_model.clone(), &self.mapping)));
            self.list_view.set_model(Some(&self.selection));

            self.list_view
                .connect_activate(clone!(@weak obj => move |_list_view, position| {
                    obj.emit_record_activated(position);
                }));

            let key_controller = gtk::EventControllerKey::new();
            key_controller.connect_key_pressed(
                clone!(@weak obj => @default-return glib::signal::Inhibit(false), move |_controller, key, _keycode, modifier| {
                    glib::signal::Inhibit(obj.on_key_press(key, modifier))
                }),
            );
            self.list_view.add_controller(&key_controller);

            self.selection.connect_selection_changed(
                clone!(@weak obj => move |selection, _pos, _n| {
                    obj.emit_selection_changed(&selection.selection())
                }),
            );

            scrolled(&self.list_view).set_parent(&*obj);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: Lazy<Vec<glib::subclass::Signal>> = Lazy::new(|| {
                vec![
                    glib::subclass::Signal::builder(SIGNAL_GO_HOME).build(),
                    glib::subclass::Signal::builder(SIGNAL_GO_UP).build(),
                    glib::subclass::Signal::builder(SIGNAL_RECORD_ACTIVATED)
                        .param_types([u32::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_SELECTION_CHANGED)
                        .param_types([gtk::Bitset::static_type()])
                        .build(),
                ]
            });
            &SIGNALS
        }

        fn dispose(&self) {
            for child in self.obj().children() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PSTreeView {}

    impl PSTreeView {
        pub fn find_record_position(&self, record: &RecordNode) -> Option<u32> {
            let model = self.selection.model()?;
            let position = model
                .iter::<glib::Object>()
                .ok()?
                .position(|r| r.as_ref() == Ok(record.upcast_ref()))?;
            position.try_into().ok()
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

impl PSTreeView {
    pub fn set_model(&self, model: &TypedListStore<RecordNode>) {
        self.imp().selection.set_model(Some(model.untyped()));
    }

    pub fn get_selected_positions(&self) -> gtk::Bitset {
        self.imp().selection.selection()
    }

    pub fn get_selected_position(&self) -> Option<u32> {
        let selection = self.get_selected_positions();
        if selection.size() != 1 {
            return None;
        }
        let pos = selection.nth(0);
        Some(pos)
    }

    pub fn set_popup(&self, popup_model: &gio::MenuModel) {
        *self.imp().popup_model.borrow_mut() = Some(popup_model.clone());
    }

    pub fn select_position(&self, position: u32) {
        self.imp().selection.select_item(position, true);
    }

    pub async fn select_record(&self, record: &RecordNode) {
        if let Some(position) = self.imp().find_record_position(record) {
            self.select_position_async(position).await;
        }
    }

    pub async fn select_position_async(&self, position: u32) {
        pending().await;

        self.imp().selection.select_item(position, true);

        pending().await;

        if let Err(err) = self
            .imp()
            .list_view
            .activate_action("list.scroll-to-item", Some(&position.to_variant()))
        {
            eprintln!("WARN: PSTreeView::select_record: {}", err);
        }

        pending().await;

        let Some(item) = self.imp().mapping.find(position) else { return };
        let Some(parent) = item.parent() else { return };
        parent.grab_focus();

        pending().await;
    }

    fn on_key_press(&self, key: gdk::Key, modifier: gdk::ModifierType) -> bool {
        match (modifier, key) {
            (gdk::ModifierType::ALT_MASK, gdk::Key::Home) => {
                self.emit_go_home();
                true
            }
            (gdk::ModifierType::ALT_MASK, gdk::Key::Up) => {
                self.emit_go_up();
                true
            }
            (gdk::ModifierType::ALT_MASK, gdk::Key::Down) => {
                if let Some(position) = self.get_selected_position() {
                    self.emit_record_activated(position);
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl PSTreeView {
    fn emit_go_home(&self) {
        self.emit_by_name::<()>(imp::SIGNAL_GO_HOME, &[]);
    }

    pub fn connect_go_home<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn() + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_GO_HOME,
            false,
            glib::closure_local!(move |_self: &Self| (f)()),
        )
    }

    fn emit_go_up(&self) {
        self.emit_by_name::<()>(imp::SIGNAL_GO_UP, &[]);
    }

    pub fn connect_go_up<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn() + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_GO_UP,
            false,
            glib::closure_local!(move |_self: &Self| (f)()),
        )
    }

    fn emit_selection_changed(&self, selection: &gtk::Bitset) {
        self.emit_by_name::<()>(imp::SIGNAL_SELECTION_CHANGED, &[selection]);
    }

    pub fn connect_selection_changed<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(gtk::Bitset) + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_SELECTION_CHANGED,
            false,
            glib::closure_local!(move |_self: &Self, selection| (f)(selection)),
        )
    }

    fn emit_record_activated(&self, position: u32) {
        self.emit_by_name::<()>(imp::SIGNAL_RECORD_ACTIVATED, &[&position]);
    }

    pub fn connect_record_activated<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(u32) + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_RECORD_ACTIVATED,
            false,
            glib::closure_local!(move |_self: &Self, position| (f)(position)),
        )
    }
}
