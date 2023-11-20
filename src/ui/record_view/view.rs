use super::item::DropOption;
use crate::model::tree::RecordNode;
use crate::utils::ui::pending;
use gtk::{gdk, gio, glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::ui::record_view::item::PSRecordViewItem;
    use crate::utils::ui::scrolled;
    use crate::weak_map::WeakMap;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::sync::OnceLock;

    pub const SIGNAL_GO_HOME: &str = "ps-go-home";
    pub const SIGNAL_GO_UP: &str = "ps-go-up";
    pub const SIGNAL_SELECTION_CHANGED: &str = "ps-selection-changed";
    pub const SIGNAL_RECORD_ACTIVATED: &str = "ps-record-activated";
    pub const SIGNAL_MOVE_RECORD: &str = "ps-record-move";

    pub struct PSRecordView {
        pub list_view: gtk::ListView,
        pub selection: gtk::MultiSelection,
        pub popup_model: Rc<RefCell<Option<gio::MenuModel>>>,
        pub mapping: Rc<WeakMap<u32, PSRecordViewItem>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSRecordView {
        const NAME: &'static str = "PSRecordView";
        type Type = super::PSRecordView;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            Self {
                list_view: Default::default(),
                selection: gtk::MultiSelection::new(None::<gio::ListModel>),
                popup_model: Default::default(),
                mapping: Default::default(),
            }
        }
    }

    impl ObjectImpl for PSRecordView {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_focusable(true);
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            let factory = gtk::SignalListItemFactory::new();
            factory.connect_setup(
                glib::clone!(@weak self as this => move |_factory, item| this.setup_item(item)),
            );
            factory.connect_bind(
                glib::clone!(@weak self as this => move |_factory, item| this.bind_item(item)),
            );
            factory.connect_unbind(
                glib::clone!(@weak self as this => move |_factory, item| this.unbind_item(item)),
            );
            factory.connect_teardown(
                glib::clone!(@weak self as this => move |_factory, item| this.teardown_item(item)),
            );
            self.list_view.set_factory(Some(&factory));
            self.list_view.set_model(Some(&self.selection));

            self.list_view.connect_activate(
                glib::clone!(@weak obj => move |_list_view, position| {
                    obj.emit_record_activated(position);
                }),
            );

            let key_controller = gtk::EventControllerKey::new();
            key_controller.connect_key_pressed(
                glib::clone!(@weak obj => @default-return glib::Propagation::Proceed, move |_controller, key, _keycode, modifier| {
                    obj.on_key_press(key, modifier)
                }),
            );
            self.list_view.add_controller(key_controller);

            self.selection.connect_selection_changed(
                glib::clone!(@weak obj => move |selection, _pos, _n| {
                    obj.emit_selection_changed(&selection.selection())
                }),
            );

            scrolled(&self.list_view).set_parent(&*obj);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    glib::subclass::Signal::builder(SIGNAL_GO_HOME).build(),
                    glib::subclass::Signal::builder(SIGNAL_GO_UP).build(),
                    glib::subclass::Signal::builder(SIGNAL_RECORD_ACTIVATED)
                        .param_types([u32::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_SELECTION_CHANGED)
                        .param_types([gtk::Bitset::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_MOVE_RECORD)
                        .param_types([
                            RecordNode::static_type(),
                            RecordNode::static_type(),
                            i8::static_type(),
                        ])
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

    impl WidgetImpl for PSRecordView {}

    impl PSRecordView {
        pub fn find_position(&self, object: &glib::Object) -> Option<u32> {
            let model = self.selection.model()?;
            let position = model.iter().position(|r| r.as_ref() == Ok(object))?;
            position.try_into().ok()
        }
    }

    impl PSRecordView {
        fn setup_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };
            let child = PSRecordViewItem::default();
            let popup_model = self.popup_model.clone();
            child.connect_context_menu(move |_record| popup_model.borrow().clone());
            let obj = self.obj();
            child.connect_move_record(glib::clone!(@weak obj => move |_, src, dst, opt| obj.emit_move_record(src, dst, opt)));
            list_item.set_child(Some(&child));
        }

        fn bind_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };
            let Some(child) = list_item.child().and_downcast::<PSRecordViewItem>() else {
                return;
            };
            let Some(record_node) = list_item.item().and_downcast::<RecordNode>() else {
                return;
            };
            let position = list_item.position();
            self.mapping.remove_value(&child);
            self.mapping.add(position, &child);
            child.set_record_node(Some((record_node, position)));
        }

        fn unbind_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };
            let Some(child) = list_item.child().and_downcast::<PSRecordViewItem>() else {
                return;
            };
            self.mapping.remove_key(list_item.position());
            self.mapping.remove_value(&child);
            child.set_record_node(None);
        }

        fn teardown_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };
            if let Some(child) = list_item.child().and_downcast::<PSRecordViewItem>() {
                child.set_record_node(None);
                self.mapping.remove_value(&child);
            }
            list_item.set_child(gtk::Widget::NONE);
        }
    }
}

glib::wrapper! {
    pub struct PSRecordView(ObjectSubclass<imp::PSRecordView>)
        @extends gtk::Widget;
}

impl Default for PSRecordView {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl PSRecordView {
    pub fn set_model(&self, model: &gio::ListModel) {
        self.imp().selection.set_model(Some(model));
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

    pub async fn select_object(&self, object: &glib::Object) {
        if let Some(position) = self.imp().find_position(object) {
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
            eprintln!("WARN: PSTreeView::select_record: {err}");
        }

        pending().await;

        let Some(item) = self.imp().mapping.find(position) else {
            return;
        };
        let Some(parent) = item.parent() else { return };
        parent.grab_focus();

        pending().await;
    }

    fn on_key_press(&self, key: gdk::Key, modifier: gdk::ModifierType) -> glib::Propagation {
        match (modifier, key) {
            (gdk::ModifierType::ALT_MASK, gdk::Key::Home) => {
                self.emit_go_home();
                glib::Propagation::Stop
            }
            (gdk::ModifierType::ALT_MASK, gdk::Key::Up) => {
                self.emit_go_up();
                glib::Propagation::Stop
            }
            (gdk::ModifierType::ALT_MASK, gdk::Key::Down) => {
                if let Some(position) = self.get_selected_position() {
                    self.emit_record_activated(position);
                    glib::Propagation::Stop
                } else {
                    glib::Propagation::Proceed
                }
            }
            _ => glib::Propagation::Proceed,
        }
    }
}

impl PSRecordView {
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

    pub fn connect_move_record<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self, &RecordNode, &RecordNode, DropOption) + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_MOVE_RECORD,
            false,
            glib::closure_local!(move |cell: &Self,
                                       src: &RecordNode,
                                       dst: &RecordNode,
                                       option: i8| {
                (f)(cell, src, dst, option.into());
            }),
        )
    }

    fn emit_move_record(&self, src: &RecordNode, dst: &RecordNode, option: DropOption) {
        let drop_option = option as i8;
        self.emit_by_name::<()>(imp::SIGNAL_MOVE_RECORD, &[&src, &dst, &drop_option]);
    }
}
