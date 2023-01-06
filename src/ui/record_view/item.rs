use crate::entropy::PasswordStrength;
use crate::gtk_prelude::*;
use crate::model::tree::RecordNode;
use crate::utils::ui::PSWidgetExt;
use gdk::ffi::{GDK_BUTTON_PRIMARY, GDK_BUTTON_SECONDARY};

mod imp {
    use super::*;
    use crate::entropy::{password_entropy, AsciiClassifier};
    use once_cell::sync::Lazy;
    use std::cell::RefCell;
    use std::collections::HashMap;

    #[derive(Default)]
    pub struct PSRecordViewItem {
        icon: gtk::Image,
        name: gtk::Label,
        strength: gtk::Image,
        open: gtk::Image,
        record: RefCell<Option<RecordNode>>,
        context_click: gtk::GestureClick,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSRecordViewItem {
        const NAME: &'static str = "PSRecordViewItem";
        type Type = super::PSRecordViewItem;
        type ParentType = gtk::Widget;
    }

    impl ObjectImpl for PSRecordViewItem {
        fn constructed(&self) {
            self.parent_constructed();

            let layout = gtk::ConstraintLayout::new();
            self.obj().set_layout_manager(Some(&layout));

            self.icon.set_parent(&*self.obj());
            self.icon.set_icon_size(gtk::IconSize::Large);

            self.name.set_parent(&*self.obj());
            self.name.set_margin_top(0);
            self.name.set_halign(gtk::Align::Start);

            self.strength.set_parent(&*self.obj());
            self.open.set_parent(&*self.obj());

            layout
                .add_constraints_from_description(
                    &[
                        "H:|-[icon(==30)]-[name]-[strength(==50)]-[share(==50)]-|",
                        "V:|-[icon]-|",
                        "V:|-[name]-|",
                        "V:|-[strength]-|",
                        "V:|-[share]-|",
                    ],
                    4,
                    0,
                    &HashMap::<&str, &gtk::Widget>::from([
                        ("icon", self.icon.upcast_ref()),
                        ("name", self.name.upcast_ref()),
                        ("strength", self.strength.upcast_ref()),
                        ("share", self.open.upcast_ref()),
                    ]),
                )
                .expect("layout is ok");

            self.context_click.set_button(GDK_BUTTON_SECONDARY as u32);
            self.context_click.connect_pressed(
                clone!(@weak self as imp => move |_gesture, _n, x, y| imp.on_context_click(x, y)),
            );
            self.obj().add_controller(&self.context_click);

            let open_click = gtk::GestureClick::builder()
                .button(GDK_BUTTON_PRIMARY as u32)
                .build();
            open_click.connect_pressed(
                clone!(@weak self as imp => move |_gesture, _n, _x, _y| imp.on_open_clicked()),
            );
            self.open.add_controller(&open_click);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: Lazy<Vec<glib::subclass::Signal>> = Lazy::new(|| {
                vec![glib::subclass::Signal::builder("context-menu")
                    .param_types([RecordNode::static_type()])
                    .return_type::<Option<gio::MenuModel>>()
                    .build()]
            });
            &SIGNALS
        }

        fn dispose(&self) {
            for child in self.obj().children() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PSRecordViewItem {}

    impl PSRecordViewItem {
        pub fn set_record_node(&self, record_node: Option<RecordNode>) {
            if let Some(ref record_node) = record_node {
                let record = record_node.record();

                self.icon.set_icon_name(Some(record.record_type.icon));

                self.name.set_text(&record.name());

                if let Some(password) = record.password() {
                    let strength = password_entropy(&AsciiClassifier, password.as_bytes()).into();
                    self.strength.set_icon_name(Some(strength_icon(strength)));
                    self.strength.set_tooltip_text(Some(strength.display()));
                } else {
                    self.strength.set_icon_name(None);
                    self.strength.set_tooltip_text(None);
                }

                if record.url().is_some() {
                    self.open.set_icon_name(Some("share"));
                } else {
                    self.open.set_icon_name(None);
                }
            } else {
                self.icon.set_icon_name(None);
                self.name.set_text("");
                self.strength.set_icon_name(None);
                self.open.set_icon_name(None);
            }
            *self.record.borrow_mut() = record_node;
        }

        fn emit_context_menu(&self, record_node: &RecordNode) -> Option<gio::MenuModel> {
            self.obj()
                .emit_by_name::<Option<gio::MenuModel>>("context-menu", &[record_node])
        }

        fn on_context_click(&self, x: f64, y: f64) {
            self.obj().grab_focus();

            let record_ref = self.record.borrow();
            let Some(record_node) = record_ref.as_ref() else { return };
            let Some(popup_model) = self.emit_context_menu(record_node) else { return };

            let context_menu = gtk::PopoverMenu::builder()
                .autohide(true)
                .menu_model(&popup_model)
                .build();
            context_menu.set_parent(&*self.obj());
            context_menu.set_pointing_to(Some(&gdk::Rectangle::new(x as i32, y as i32, 0, 0)));
            context_menu.popup();
        }

        fn on_open_clicked(&self) {
            self.obj().grab_focus();

            let record_ref = self.record.borrow();
            if let Some(url) = record_ref.as_ref().and_then(|n| n.record().url()) {
                gtk::show_uri(self.root_window().as_ref(), url, 0);
            }
        }

        fn root_window(&self) -> Option<gtk::Window> {
            self.obj()
                .root()
                .and_then(|r| r.downcast::<gtk::Window>().ok())
        }
    }
}

glib::wrapper! {
    pub struct PSRecordViewItem(ObjectSubclass<imp::PSRecordViewItem>)
        @extends gtk::Widget;
}

impl PSRecordViewItem {
    pub fn new() -> Self {
        glib::Object::builder().build()
    }

    pub fn set_record_node(&self, record_node: Option<RecordNode>) {
        self.imp().set_record_node(record_node);
    }

    pub fn connect_context_menu<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&RecordNode) -> Option<gio::MenuModel> + 'static,
    {
        self.connect_closure(
            "context-menu",
            false,
            glib::closure_local!(move |_self: &Self, node: &RecordNode| (f)(node)),
        )
    }
}

fn strength_icon(strength: PasswordStrength) -> &'static str {
    match strength {
        PasswordStrength::VeryWeak => "strength-very-weak",
        PasswordStrength::Weak => "strength-weak",
        PasswordStrength::Reasonable => "strength-reasonable",
        PasswordStrength::Strong => "strength-strong",
        PasswordStrength::VeryStrong => "strength-very-strong",
    }
}
