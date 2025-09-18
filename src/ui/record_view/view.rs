use super::item::DropOption;
use crate::{
    model::tree::{RecordNode, RecordTree},
    utils::ui::{pending, pending_idle},
};
use gtk::{gdk, gio, glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::search::item::SearchMatch;
    use crate::ui::record_view::item::PSRecordViewItem;
    use crate::ui::search_bar::{PSSearchBar, SearchEvent, SearchEventType};
    use crate::utils::grid_layout::PSGridLayoutExt;
    use crate::utils::typed_list_store::TypedListStore;
    use crate::utils::ui::{orphan_all_children, scrolled};
    use crate::weak_map::WeakMap;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::sync::OnceLock;

    pub const SIGNAL_SELECTION_CHANGED: &str = "ps-selection-changed";
    pub const SIGNAL_RECORD_ACTIVATED: &str = "ps-record-activated";
    pub const SIGNAL_MOVE_RECORD: &str = "ps-record-move";

    pub struct PSRecordView {
        pub model: RefCell<RecordTree>,
        pub search_result: TypedListStore<SearchMatch>,
        pub search_bar: PSSearchBar,
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
                model: Default::default(),
                search_result: Default::default(),
                search_bar: Default::default(),
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
            obj.set_layout_manager(Some(gtk::GridLayout::new()));

            let factory = gtk::SignalListItemFactory::new();
            factory.connect_setup(glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |_factory, item| this.setup_item(item)
            ));
            factory.connect_bind(glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |_factory, item| this.bind_item(item)
            ));
            factory.connect_unbind(glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |_factory, item| this.unbind_item(item)
            ));
            factory.connect_teardown(glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |_factory, item| this.teardown_item(item)
            ));
            self.list_view.set_factory(Some(&factory));
            self.list_view.set_model(Some(&self.selection));

            self.list_view.connect_activate(glib::clone!(
                #[weak]
                obj,
                move |list_view, position| {
                    if let Some(item) = list_view
                        .model()
                        .and_then(|m| m.item(position))
                        .and_downcast::<gtk::TreeListRow>()
                    {
                        if item.is_expandable() {
                            item.set_expanded(!item.is_expanded());
                        } else if let Some(record_node) = item.item().and_downcast::<RecordNode>() {
                            obj.emit_record_activated(position, &record_node);
                        } else {
                            // TODO: warn unreachable
                        }
                    } else {
                        // TODO: warn unreachable
                    }
                }
            ));

            let key_controller = gtk::EventControllerKey::new();
            key_controller.connect_key_pressed(glib::clone!(
                #[weak]
                obj,
                #[upgrade_or]
                glib::Propagation::Proceed,
                move |_controller, key, _keycode, modifier| obj.on_key_press(key, modifier)
            ));
            self.list_view.add_controller(key_controller);

            self.selection.connect_selection_changed(glib::clone!(
                #[weak]
                obj,
                move |selection, _pos, _n| obj.emit_selection_changed(&selection.selection())
            ));

            obj.grid_attach(&self.search_bar).set_row(0);
            obj.grid_attach(&scrolled(&self.list_view)).set_row(1);

            self.search_bar.connect_search(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |event| {
                    glib::spawn_future_local(async move {
                        imp.search(&event).await;
                    });
                }
            ));
            // self.search_bar.connect_configure(glib::clone!(
            //     #[weak(rename_to = imp)]
            //     self,
            //     move |search_config| {
            //         imp.config_service.get().unwrap().update(|config| {
            //             config.search_in_secrets = search_config.search_in_secrets;
            //         });
            //     }
            // ));
            self.search_bar.connect_search_closed(glib::clone!(
                #[weak]
                obj,
                move || obj.search_reset()
            ));
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    glib::subclass::Signal::builder(SIGNAL_RECORD_ACTIVATED)
                        .param_types([u32::static_type(), RecordNode::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_SELECTION_CHANGED)
                        .param_types([gtk::Bitset::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_MOVE_RECORD)
                        .param_types([
                            glib::Object::static_type(),
                            glib::Object::static_type(),
                            i8::static_type(),
                        ])
                        .build(),
                ]
            })
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
        }
    }

    impl WidgetImpl for PSRecordView {}

    impl PSRecordView {
        fn find_selection_in_search_result(&self) -> Option<usize> {
            self.obj()
                .get_selected_position()
                .and_then(|position| self.obj().record_at(position))
                .and_then(|record_node| {
                    self.search_result
                        .iter()
                        .position(|sm| *sm.record() == record_node)
                })
        }

        async fn search(&self, event: &SearchEvent) {
            if event.query.is_empty() {
                return;
            }

            fn traverse(
                records: &TypedListStore<RecordNode>,
                path: TypedListStore<RecordNode>,
                query: &str,
                search_in_secrets: bool,
                result: &TypedListStore<SearchMatch>,
            ) {
                for record in records {
                    if record.record().has_text(query, search_in_secrets) {
                        result.append(&SearchMatch::new(&record, &path.clone_list()));
                    }
                    if let Some(children) = record.children() {
                        let path_to_record = path.appended(record.clone());
                        traverse(children, path_to_record, query, search_in_secrets, result);
                    }
                }
            }

            let search_match_index = match event.event_type {
                SearchEventType::Change => {
                    self.search_result.remove_all();
                    traverse(
                        &self.model.borrow().records,
                        Default::default(),
                        &event.query,
                        event.search_in_secrets,
                        &self.search_result,
                    );
                    Some(0)
                }
                SearchEventType::Next => self
                    .find_selection_in_search_result()
                    .map(|index| (index as u32 + 1).min(self.search_result.len())),
                SearchEventType::Prev => self
                    .find_selection_in_search_result()
                    .map(|index| (index as u32).saturating_sub(1)),
            };
            let Some(search_match) =
                search_match_index.and_then(|index| self.search_result.get(index))
            else {
                return;
            };

            self.expand_path(search_match.path()).await;
            self.obj().select_record(search_match.record(), false).await;
        }

        async fn expand_path(&self, path: &TypedListStore<RecordNode>) -> bool {
            for record_node in path {
                let Some((_, row)) = self.obj().find_record(&record_node) else {
                    return false;
                };
                row.set_expanded(true);
                pending().await;
            }
            true
        }
    }

    impl PSRecordView {
        fn setup_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };

            let child = PSRecordViewItem::default();
            child.set_hexpand(true);

            let expander = gtk::TreeExpander::builder()
                .indent_for_depth(true)
                .indent_for_icon(true)
                .hexpand(true)
                .child(&child)
                .build();
            expander.add_css_class("huge-tree-indent");

            let popup_model = self.popup_model.clone();
            child.connect_context_menu(move |_record| popup_model.borrow().clone());
            let obj = self.obj();
            child.connect_move_record(glib::clone!(
                #[weak]
                obj,
                move |_, src, dst, opt| obj.emit_move_record(src, dst, opt)
            ));
            list_item.set_child(Some(&expander));
        }

        fn bind_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };
            let Some(expander) = list_item.child().and_downcast::<gtk::TreeExpander>() else {
                return;
            };
            let Some(child) = expander.child().and_downcast::<PSRecordViewItem>() else {
                return;
            };
            let Some(tree_list_row) = list_item.item().and_downcast::<gtk::TreeListRow>() else {
                return;
            };
            expander.set_list_row(Some(&tree_list_row));
            let Some(record_node) = tree_list_row.item().and_downcast::<RecordNode>() else {
                return;
            };
            let position = list_item.position();
            self.mapping.remove_value(&child);
            self.mapping.add(position, &child);
            child.set_record_node(Some((self.model.borrow().clone(), record_node, position)));
        }

        fn unbind_item(&self, item: &glib::Object) {
            let Some(list_item) = item.downcast_ref::<gtk::ListItem>() else {
                return;
            };
            let Some(expander) = list_item.child().and_downcast::<gtk::TreeExpander>() else {
                return;
            };
            expander.set_list_row(None);
            let Some(child) = expander.child().and_downcast::<PSRecordViewItem>() else {
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
            let Some(expander) = list_item.child().and_downcast::<gtk::TreeExpander>() else {
                return;
            };
            if let Some(child) = expander.child().and_downcast::<PSRecordViewItem>() {
                child.set_record_node(None);
                self.mapping.remove_value(&child);
            }
            list_item.set_child(gtk::Widget::NONE);
        }
    }
}

glib::wrapper! {
    pub struct PSRecordView(ObjectSubclass<imp::PSRecordView>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for PSRecordView {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl PSRecordView {
    pub fn set_model(&self, model: &RecordTree) {
        *self.imp().model.borrow_mut() = model.clone();
        self.imp().search_result.remove_all();

        let tree_model =
            gtk::TreeListModel::new(model.records.untyped().clone(), false, false, |node| {
                let r = node.downcast_ref::<RecordNode>()?;
                let children = r.children()?;
                Some(children.untyped().clone().upcast())
            });
        self.imp().selection.set_model(Some(&tree_model));
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

    pub fn record_at(&self, position: u32) -> Option<RecordNode> {
        let model = self.imp().selection.model()?;
        let tree_list_row = model.item(position).and_downcast::<gtk::TreeListRow>()?;
        let record = tree_list_row.item().and_downcast::<RecordNode>()?;
        Some(record)
    }

    pub fn find_record(&self, record_node: &RecordNode) -> Option<(u32, gtk::TreeListRow)> {
        self.imp()
            .selection
            .model()?
            .iter::<gtk::TreeListRow>()
            .enumerate()
            .find(|(_position, result)| {
                result
                    .as_ref()
                    .ok()
                    .and_then(|row| row.item())
                    .and_then(|item| item.downcast::<RecordNode>().ok())
                    .as_ref()
                    == Some(record_node)
            })
            .and_then(|(position, result)| Some((position.try_into().ok()?, result.ok()?)))
    }

    pub fn set_popup(&self, popup_model: &gio::MenuModel) {
        *self.imp().popup_model.borrow_mut() = Some(popup_model.clone());
    }

    pub fn select_position(&self, position: u32) {
        self.imp().selection.select_item(position, true);
    }

    pub async fn select_record(&self, record_node: &RecordNode, grab_focus: bool) {
        if let Some((position, _)) = self.find_record(record_node) {
            self.select_position_async(position, grab_focus).await;
        }
    }

    pub async fn select_position_async(&self, position: u32, grab_focus: bool) {
        pending().await;

        self.imp().list_view.scroll_to(
            position,
            gtk::ListScrollFlags::FOCUS | gtk::ListScrollFlags::SELECT,
            None,
        );

        pending_idle().await;

        self.imp().list_view.scroll_to(
            position,
            gtk::ListScrollFlags::FOCUS | gtk::ListScrollFlags::SELECT,
            None,
        );

        pending_idle().await;

        if grab_focus
            && let Some(item) = self.imp().mapping.find(position)
            && let Some(parent) = item.parent()
        {
            parent.grab_focus();
            pending().await;
        }
    }

    fn on_key_press(&self, key: gdk::Key, modifier: gdk::ModifierType) -> glib::Propagation {
        match (modifier, key) {
            (gdk::ModifierType::ALT_MASK, gdk::Key::Down) => self
                .get_selected_position()
                .and_then(|position| {
                    let record = self.record_at(position)?;
                    self.emit_record_activated(position, &record);
                    Some(glib::Propagation::Stop)
                })
                .unwrap_or(glib::Propagation::Proceed),
            _ => glib::Propagation::Proceed,
        }
    }

    pub fn search_start(&self) {
        self.imp().search_bar.set_search_mode(true);
    }

    pub fn search_reset(&self) {
        self.imp().search_bar.set_search_mode(false);
        self.imp().search_bar.reset();
    }
}

impl PSRecordView {
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

    fn emit_record_activated(&self, position: u32, record_node: &RecordNode) {
        self.emit_by_name::<()>(imp::SIGNAL_RECORD_ACTIVATED, &[&position, record_node]);
    }

    pub fn connect_record_activated<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(u32, RecordNode) + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_RECORD_ACTIVATED,
            false,
            glib::closure_local!(move |_self: &Self, position, record_node| (f)(
                position,
                record_node
            )),
        )
    }

    fn emit_move_record(&self, src: &RecordNode, dst: &RecordNode, option: DropOption) {
        let drop_option = option as i8;
        self.emit_by_name::<()>(imp::SIGNAL_MOVE_RECORD, &[&src, &dst, &drop_option]);
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
}
