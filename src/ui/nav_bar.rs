use gtk::{gio, glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::model::tree::RecordNode;
    use crate::ui::list_item_factory::PSListItemFactory;
    use crate::utils::grid_layout::PSGridLayoutExt;
    use crate::utils::ui::orphan_all_children;
    use std::cell::Cell;
    use std::sync::OnceLock;

    struct ItemFactory;

    impl PSListItemFactory for ItemFactory {
        type Child = gtk::Label;

        fn setup(&self) -> gtk::Label {
            gtk::Label::builder()
                .xalign(0.0_f32)
                .yalign(0.5_f32)
                .hexpand(true)
                .margin_top(5)
                .margin_bottom(5)
                .margin_start(5)
                .margin_end(5)
                .build()
        }

        fn bind(&self, list_item: &gtk::ListItem, label: &gtk::Label) {
            if let Some(record_node) = list_item.item().and_downcast::<RecordNode>() {
                label.set_label(&record_node.record().name());
            } else {
                label.set_label("");
            }
        }

        fn unbind(&self, _list_item: &gtk::ListItem, label: &gtk::Label) {
            label.set_label("");
        }
    }

    pub const SIGNAL_GO_HOME: &str = "ps-go-home";
    pub const SIGNAL_GO_PATH: &str = "ps-go-path";
    pub const SIGNAL_GO_UP: &str = "ps-go-up";

    pub struct PSNavBar {
        pub home_button: gtk::Button,
        pub list_view: gtk::ListView,
        pub selection: gtk::SingleSelection,
        pub up_button: gtk::Button,
        pub model_change_handler: Cell<Option<glib::signal::SignalHandlerId>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSNavBar {
        const NAME: &'static str = "PSNavBar";
        type Type = super::PSNavBar;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            let home_button = gtk::Button::builder()
                .icon_name("navigate-home")
                .tooltip_text("Go go the root")
                .sensitive(false)
                .hexpand(false)
                .build();
            home_button.add_css_class("flat");

            let selection = gtk::SingleSelection::new(None::<gio::ListModel>);

            let list_view = gtk::ListView::builder()
                .orientation(gtk::Orientation::Horizontal)
                .factory(&ItemFactory.into_factory())
                .model(&selection)
                .show_separators(true)
                .build();

            let up_button = gtk::Button::builder()
                .icon_name("navigate-up")
                .tooltip_text("Go go parent group")
                .sensitive(false)
                .hexpand(false)
                .build();
            up_button.add_css_class("flat");

            Self {
                home_button,
                list_view,
                selection,
                up_button,
                model_change_handler: Cell::new(None),
            }
        }
    }

    impl ObjectImpl for PSNavBar {
        fn constructed(&self) {
            self.parent_constructed();

            let layout = gtk::GridLayout::builder().column_spacing(5).build();

            let obj = self.obj();
            obj.set_layout_manager(Some(layout));
            obj.set_margin_top(5);
            obj.set_margin_bottom(5);
            obj.set_margin_start(5);
            obj.set_margin_end(5);

            self.home_button.connect_clicked(glib::clone!(
                #[weak]
                obj,
                move |_| {
                    obj.emit_go_home();
                }
            ));
            obj.grid_attach(&self.home_button).set_column(0);

            let sw = gtk::ScrolledWindow::builder()
                .hscrollbar_policy(gtk::PolicyType::Automatic)
                .vscrollbar_policy(gtk::PolicyType::Never)
                .has_frame(true)
                .hexpand(true)
                .vexpand(true)
                .child(&self.list_view)
                .build();
            obj.grid_attach(&sw).set_column(1);

            self.list_view.connect_activate(glib::clone!(
                #[weak]
                obj,
                move |_, position| {
                    obj.emit_go_path(position);
                }
            ));

            self.up_button.connect_clicked(glib::clone!(
                #[weak]
                obj,
                move |_| {
                    obj.emit_go_up();
                }
            ));
            obj.grid_attach(&self.up_button).set_column(2);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    glib::subclass::Signal::builder(SIGNAL_GO_HOME).build(),
                    glib::subclass::Signal::builder(SIGNAL_GO_PATH)
                        .param_types([u32::static_type()])
                        .build(),
                    glib::subclass::Signal::builder(SIGNAL_GO_UP).build(),
                ]
            })
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
            self.disconnect_model_change_handler();
        }
    }

    impl WidgetImpl for PSNavBar {}

    impl PSNavBar {
        pub fn disconnect_model_change_handler(&self) {
            if let Some(handler_id) = self.model_change_handler.take() {
                if let Some(model) = self.selection.model() {
                    model.disconnect(handler_id);
                }
            }
        }

        pub fn model_changed(&self, model: &gio::ListModel) {
            let size = model.n_items();
            self.selection.select_item(size.saturating_sub(1), true);

            let has_parent = size > 0;
            self.home_button.set_sensitive(has_parent);
            self.up_button.set_sensitive(has_parent);
        }
    }
}

glib::wrapper! {
    pub struct PSNavBar(ObjectSubclass<imp::PSNavBar>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for PSNavBar {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl PSNavBar {
    pub fn set_model(&self, model: &gio::ListModel) {
        self.imp().selection.set_model(Some(model));
        self.imp().disconnect_model_change_handler();
        self.imp()
            .model_change_handler
            .set(Some(model.connect_items_changed(glib::clone!(
                #[weak(rename_to = this)]
                self,
                move |model, _, _, _| {
                    this.imp().model_changed(model);
                }
            ))));
        self.imp().model_changed(model);
    }
}

impl PSNavBar {
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

    fn emit_go_path(&self, position: u32) {
        self.emit_by_name::<()>(imp::SIGNAL_GO_PATH, &[&position]);
    }

    pub fn connect_go_path<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(u32) + 'static,
    {
        self.connect_closure(
            imp::SIGNAL_GO_PATH,
            false,
            glib::closure_local!(move |_self: &Self, position| (f)(position)),
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
}
