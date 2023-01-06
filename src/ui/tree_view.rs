use crate::gtk_prelude::*;
use crate::model::tree::RecordNode;
use crate::utils::typed_list_store::TypedListStore;
use std::cell::RefCell;
use std::rc::Rc;

mod imp {
    use super::*;
    use crate::ui::record_view::item_factory::item_factory;
    use crate::utils::ui::{scrolled, PSWidgetExt};

    pub struct PSTreeView {
        pub list_view: gtk::ListView,
        pub selection: gtk::MultiSelection,
        pub popup_model: Rc<RefCell<Option<gio::MenuModel>>>,
    }

    impl Default for PSTreeView {
        fn default() -> Self {
            Self {
                list_view: Default::default(),
                selection: gtk::MultiSelection::new(gio::ListModel::NONE),
                popup_model: Default::default(),
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
                .set_factory(Some(&item_factory(self.popup_model.clone())));
            self.list_view.set_model(Some(&self.selection));
            scrolled(&self.list_view).set_parent(&*obj);
        }

        fn dispose(&self) {
            for child in self.obj().children() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PSTreeView {}
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

    pub fn connect_record_changed<F: Fn(gtk::Bitset) + 'static>(&self, changed: F) {
        self.imp()
            .selection
            .connect_selection_changed(move |selection, _pos, _n| {
                changed(selection.selection());
            });
    }

    pub fn connect_record_activated<F: Fn(u32) + 'static>(&self, activated: F) {
        self.imp()
            .list_view
            .connect_activate(move |_list_view, position| {
                activated(position);
            });
    }

    pub fn set_popup(&self, popup_model: &gio::MenuModel) {
        *self.imp().popup_model.borrow_mut() = Some(popup_model.clone());
    }

    pub fn select_position(&self, position: u32) {
        self.imp().selection.select_item(position, true);
    }
}
