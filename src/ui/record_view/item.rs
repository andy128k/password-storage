use super::has_record::{HasRecord, PSRecordViewOptions};
use crate::entropy::PasswordStrength;
use crate::utils::style::PSStyleContextExt;
use gtk::{
    gdk,
    gdk::ffi::{GDK_BUTTON_PRIMARY, GDK_BUTTON_SECONDARY},
    gio, glib,
    prelude::*,
    subclass::prelude::*,
};

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum DropOption {
    Above = -1,
    Into = 0,
    Below = 1,
}

impl From<i8> for DropOption {
    fn from(value: i8) -> Self {
        use std::cmp::Ordering::*;
        match value.cmp(&0) {
            Less => Self::Above,
            Equal => Self::Into,
            Greater => Self::Below,
        }
    }
}

pub struct MoveRecord {
    pub src: glib::Object,
    pub dst: glib::Object,
    pub option: DropOption,
}

mod imp {
    use super::*;
    use crate::entropy::{password_entropy, AsciiClassifier};
    use crate::ui::dialogs::show_uri::show_uri;
    use crate::ui::record_view::compose_paintable::PSBackgroundPaintable;
    use crate::utils::style::StaticCssExt;
    use crate::utils::ui::orphan_all_children;
    use std::cell::{Cell, OnceCell, RefCell};
    use std::sync::OnceLock;

    #[derive(Default)]
    pub struct PSRecordViewItem {
        icon: gtk::Image,
        name: gtk::Label,
        strength: gtk::Image,
        open: gtk::Image,
        record: RefCell<Option<glib::Object>>,
        pub has_record: OnceCell<&'static dyn HasRecord>,
        pub position: Cell<Option<u32>>,
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

            let obj = self.obj();

            obj.add_static_css(
                include_str!("item.css"),
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
            obj.add_css_class("ps-list-item");

            let layout = gtk::BoxLayout::builder()
                .orientation(gtk::Orientation::Horizontal)
                .spacing(4)
                .build();
            obj.set_layout_manager(Some(layout));

            self.icon.set_width_request(30);
            self.icon.set_parent(&*obj);
            self.icon.set_icon_size(gtk::IconSize::Large);

            self.name.set_parent(&*obj);
            self.name.set_hexpand(true);
            self.name.set_margin_top(0);
            self.name.set_halign(gtk::Align::Start);

            self.strength.set_width_request(50);
            self.strength.set_parent(&*obj);

            self.open.set_width_request(50);
            self.open.set_parent(&*obj);

            self.context_click.set_button(GDK_BUTTON_SECONDARY as u32);
            self.context_click.connect_pressed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_gesture, _n, x, y| imp.on_context_click(x, y)
            ));
            obj.add_controller(self.context_click.clone());

            let open_click = gtk::GestureClick::builder()
                .button(GDK_BUTTON_PRIMARY as u32)
                .build();
            open_click.connect_pressed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_gesture, _n, _x, _y| {
                    glib::MainContext::default()
                        .spawn_local(async move { imp.on_open_clicked().await });
                }
            ));
            self.open.add_controller(open_click);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    glib::subclass::Signal::builder("context-menu")
                        .param_types([glib::Object::static_type()])
                        .return_type::<Option<gio::MenuModel>>()
                        .build(),
                    glib::subclass::Signal::builder("move-record")
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

    impl WidgetImpl for PSRecordViewItem {}

    impl PSRecordViewItem {
        fn record_type(&self) -> &'static dyn HasRecord {
            *self.has_record.get().unwrap()
        }

        pub fn setup_drag_and_drop(&self) {
            let drag_source = gtk::DragSource::builder()
                .actions(gdk::DragAction::MOVE)
                .build();
            drag_source.connect_prepare(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                None,
                move |_source, _x, _y| imp.drag_prepare()
            ));
            drag_source.connect_drag_begin(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |source, _drag| source.set_icon(imp.drag_begin().as_ref(), 0, 0)
            ));
            self.obj().add_controller(drag_source);

            let drop_target =
                gtk::DropTarget::new(self.record_type().get_type(), gdk::DragAction::MOVE);
            drop_target.set_preload(true);
            drop_target.connect_enter(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                gdk::DragAction::empty(),
                move |target, x, y| imp.drop_motion(target, x, y)
            ));
            drop_target.connect_motion(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                gdk::DragAction::empty(),
                move |target, x, y| imp.drop_motion(target, x, y)
            ));
            drop_target.connect_leave(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_target| imp.set_drop_style(None)
            ));
            drop_target.connect_drop(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                false,
                move |_target, value, x, y| imp.drop(value, x, y)
            ));
            self.obj().add_controller(drop_target);
        }

        pub fn set_record_node(&self, record_node: Option<glib::Object>) {
            if let Some(ref record_node) = record_node {
                let record = self.record_type().get_record(record_node);

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

        fn emit_context_menu(&self, record_node: &glib::Object) -> Option<gio::MenuModel> {
            self.obj()
                .emit_by_name::<Option<gio::MenuModel>>("context-menu", &[record_node])
        }

        fn on_context_click(&self, x: f64, y: f64) {
            self.obj().grab_focus();

            let record_ref = self.record.borrow();
            let Some(record_node) = record_ref.as_ref() else {
                return;
            };
            let Some(popup_model) = self.emit_context_menu(record_node) else {
                return;
            };

            let context_menu = gtk::PopoverMenu::builder()
                .autohide(true)
                .menu_model(&popup_model)
                .build();
            context_menu.set_parent(&*self.obj());
            context_menu.set_pointing_to(Some(&gdk::Rectangle::new(x as i32, y as i32, 0, 0)));
            context_menu.popup();
        }

        fn record_url(&self) -> Option<String> {
            let record = self.record.borrow();
            let url = self.record_type().get_record(record.as_ref()?).url()?;
            Some(url.to_string())
        }

        async fn on_open_clicked(&self) {
            self.obj().grab_focus();

            if let Some(url) = self.record_url() {
                show_uri(self.root_window().as_ref(), &url).await;
            }
        }

        fn root_window(&self) -> Option<gtk::Window> {
            self.obj().root().and_downcast::<gtk::Window>()
        }

        fn drag_prepare(&self) -> Option<gdk::ContentProvider> {
            let record_ref = self.record.borrow();
            let value = record_ref.as_ref()?.to_value();
            Some(gdk::ContentProvider::for_value(&value))
        }

        fn drag_begin(&self) -> Option<gdk::Paintable> {
            let obj = self.obj();
            let paintable = gtk::WidgetPaintable::new(Some(&*obj));
            let paintable = PSBackgroundPaintable::new(paintable);
            Some(paintable.upcast())
        }

        fn drop_motion(&self, target: &gtk::DropTarget, x: f64, y: f64) -> gdk::DragAction {
            let Some(value) = target.value() else {
                return gdk::DragAction::empty();
            };
            match self.drop_result(&value, x, y) {
                Some(move_record) => {
                    self.set_drop_style(Some(move_record.option));
                    gdk::DragAction::MOVE
                }
                None => {
                    self.set_drop_style(None);
                    gdk::DragAction::empty()
                }
            }
        }

        fn drop_result(&self, value: &glib::Value, _x: f64, y: f64) -> Option<MoveRecord> {
            let src = value.get::<glib::Object>().ok()?;

            let record_ref = self.record.borrow();
            let dst = record_ref.as_ref()?;
            if src == *dst {
                return None;
            }

            let height = f64::from(self.obj().height());
            let option = if self.record_type().get_record(dst).record_type.is_group {
                if y < height / 3.0 && self.position.get() == Some(0) {
                    DropOption::Above
                } else if y < height * 2.0 / 3.0 {
                    DropOption::Into
                } else {
                    DropOption::Below
                }
            } else {
                if y < height / 2.0 && self.position.get() == Some(0) {
                    DropOption::Above
                } else {
                    DropOption::Below
                }
            };

            Some(MoveRecord {
                src,
                dst: dst.clone(),
                option,
            })
        }

        fn set_drop_style(&self, drop_option: Option<DropOption>) {
            self.obj()
                .set_css_class("drop-above", drop_option == Some(DropOption::Above));
            self.obj()
                .set_css_class("drop-below", drop_option == Some(DropOption::Below));
            self.obj()
                .set_css_class("drop-into", drop_option == Some(DropOption::Into));
        }

        fn drop(&self, value: &glib::Value, x: f64, y: f64) -> bool {
            if let Some(move_record) = self.drop_result(value, x, y) {
                self.obj().emit_move_record(move_record);
            }
            false
        }
    }
}

glib::wrapper! {
    pub struct PSRecordViewItem(ObjectSubclass<imp::PSRecordViewItem>)
        @extends gtk::Widget;
}

impl PSRecordViewItem {
    pub fn new(options: PSRecordViewOptions) -> Self {
        let obj: Self = glib::Object::builder().build();
        obj.imp().has_record.set(options.has_record).ok().unwrap();
        if options.drag_and_drop {
            obj.imp().setup_drag_and_drop();
        }
        obj
    }

    pub fn set_record_node(&self, record_node: Option<(glib::Object, u32)>) {
        let (record_node, position) = record_node.unzip();
        self.imp().set_record_node(record_node);
        self.imp().position.set(position);
    }

    pub fn connect_context_menu<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&glib::Object) -> Option<gio::MenuModel> + 'static,
    {
        self.connect_closure(
            "context-menu",
            false,
            glib::closure_local!(move |_self: &Self, node: &glib::Object| (f)(node)),
        )
    }

    pub fn connect_move_record<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self, &glib::Object, &glib::Object, DropOption) + 'static,
    {
        self.connect_closure(
            "move-record",
            false,
            glib::closure_local!(move |cell: &Self,
                                       src: &glib::Object,
                                       dst: &glib::Object,
                                       option: i8| {
                (f)(cell, src, dst, option.into());
            }),
        )
    }

    fn emit_move_record(&self, move_record: MoveRecord) {
        let drop_option = move_record.option as i8;
        self.emit_by_name::<()>(
            "move-record",
            &[&move_record.src, &move_record.dst, &drop_option],
        );
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
