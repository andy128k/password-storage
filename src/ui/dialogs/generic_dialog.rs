use crate::{compat::accel::PRIMARY_MODIFIER, utils::channel::SenderExt};
use gtk::{gdk, glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::utils::ui::title;
    use std::cell::{Cell, RefCell};

    #[derive(glib::Properties)]
    #[properties(wrapper_type = super::GenericDialog)]
    pub struct GenericDialog {
        pub title: gtk::Label,
        vbox: gtk::Box,
        #[property(get, set = Self::set_content, nullable)]
        content: RefCell<Option<gtk::Widget>>,
        pub cancel_button: gtk::Button,
        pub ok_button: gtk::Button,
        #[property(get, set)]
        response: Cell<i32>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for GenericDialog {
        const NAME: &'static str = "PSGenericDialog";
        type Type = super::GenericDialog;
        type ParentType = gtk::Window;

        fn class_init(klass: &mut Self::Class) {
            klass.add_shortcut(&gtk::Shortcut::new(
                Some(close_dialog_trigger()),
                Some(gtk::CallbackAction::new(|widget, _| {
                    let response: i32 = gtk::ResponseType::Cancel.into();
                    widget.set_property("response", response);
                    glib::Propagation::Stop
                })),
            ));
        }

        fn new() -> Self {
            let title = title("");

            let cancel_button = gtk::Button::builder()
                .label("Cancel")
                .hexpand(true)
                .halign(gtk::Align::End)
                .build();

            let ok_button = gtk::Button::builder()
                .label("OK")
                .receives_default(true)
                .build();
            ok_button.add_css_class("suggested-action");

            let button_group = gtk::SizeGroup::new(gtk::SizeGroupMode::Horizontal);
            button_group.add_widget(&cancel_button);
            button_group.add_widget(&ok_button);

            Self {
                title,
                vbox: gtk::Box::builder()
                    .orientation(gtk::Orientation::Vertical)
                    .margin_top(12)
                    .margin_bottom(12)
                    .margin_start(12)
                    .margin_end(12)
                    .spacing(12)
                    .build(),
                content: Default::default(),
                cancel_button,
                ok_button,
                response: Cell::new(gtk::ResponseType::None.into()),
            }
        }
    }

    #[glib::derived_properties]
    impl ObjectImpl for GenericDialog {
        fn constructed(&self) {
            self.parent_constructed();

            let header = gtk::HeaderBar::builder()
                .title_widget(&self.title)
                .show_title_buttons(false)
                .build();

            let button_box = gtk::Box::builder()
                .orientation(gtk::Orientation::Horizontal)
                .spacing(6)
                .build();
            self.vbox.append(&button_box);

            self.cancel_button.connect_clicked(glib::clone!(
                #[weak(rename_to = this)]
                self.obj(),
                move |_| this.set_response(i32::from(gtk::ResponseType::Cancel))
            ));
            button_box.append(&self.cancel_button);

            self.ok_button.connect_clicked(glib::clone!(
                #[weak(rename_to = this)]
                self.obj(),
                move |_| this.set_response(i32::from(gtk::ResponseType::Ok))
            ));
            button_box.append(&self.ok_button);

            self.obj().set_modal(true);
            self.obj().set_resizable(true);
            self.obj().set_titlebar(Some(&header));
            self.obj().set_icon_name(Some("password-storage"));
            self.obj().set_child(Some(&self.vbox));

            let key_controller = gtk::EventControllerKey::new();
            key_controller.connect_key_pressed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                glib::Propagation::Proceed,
                move |_controller, key, _keycode, modifier| {
                    const NO_MODIFIER: gdk::ModifierType = gdk::ModifierType::empty();
                    match (key, modifier) {
                        (gdk::Key::Return, NO_MODIFIER) => {
                            imp.ok_button.activate();
                            glib::Propagation::Stop
                        }
                        _ => glib::Propagation::Proceed,
                    }
                }
            ));
            self.obj().add_controller(key_controller);

            self.obj().connect_close_request(|this| {
                this.set_response(i32::from(gtk::ResponseType::DeleteEvent));
                glib::Propagation::Proceed
            });
        }
    }

    impl WidgetImpl for GenericDialog {}
    impl WindowImpl for GenericDialog {}

    impl GenericDialog {
        fn set_content(&self, content: Option<gtk::Widget>) {
            if let Some(old_content) = self.content.take() {
                self.vbox.remove(&old_content);
            }
            if let Some(ref content) = content {
                content.set_vexpand(true);
                self.vbox.prepend(content);
            }
            self.content.replace(content);
        }
    }

    fn close_dialog_trigger() -> gtk::ShortcutTrigger {
        gtk::AlternativeTrigger::new(
            gtk::KeyvalTrigger::new(gdk::Key::Escape, gdk::ModifierType::empty()),
            gtk::AlternativeTrigger::new(
                gtk::KeyvalTrigger::new(gdk::Key::w, PRIMARY_MODIFIER),
                gtk::KeyvalTrigger::new(gdk::Key::W, PRIMARY_MODIFIER),
            ),
        )
        .upcast()
    }
}

glib::wrapper! {
    pub struct GenericDialog(ObjectSubclass<imp::GenericDialog>)
        @extends gtk::Widget, gtk::Window,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget, gtk::Root, gtk::Native, gtk::ShortcutManager;
}

impl Default for GenericDialog {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl GenericDialog {
    pub fn set_title(&self, title: &str) {
        self.imp().title.set_label(title);
    }

    pub fn set_ok_label(&self, label: &str) {
        self.imp().ok_button.set_label(label);
    }

    pub fn set_ok_sensitive(&self, sensitive: bool) {
        self.imp().ok_button.set_sensitive(sensitive);
    }

    pub async fn run(&self) -> Option<gtk::ResponseType> {
        let (sender, receiver) = async_channel::bounded::<gtk::ResponseType>(1);
        self.connect_response_notify(move |this| {
            sender.toss(this.response().into());
        });
        self.present();
        let result = receiver.recv().await.ok();
        self.set_visible(false);
        result
    }
}
