use crate::compat::accel::PRIMARY_MODIFIER;
use gtk::{gdk, glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::utils::ui::title;
    use futures::channel::mpsc::{channel, Receiver, Sender};
    use futures::stream::StreamExt;
    use std::cell::RefCell;

    pub struct GenericDialog {
        pub title: gtk::Label,
        pub cancel_button: gtk::Button,
        pub ok_button: gtk::Button,
        pub sender: RefCell<Option<Sender<gtk::ResponseType>>>,
        pub receiver: RefCell<Option<Receiver<gtk::ResponseType>>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for GenericDialog {
        const NAME: &'static str = "PSGenericDialog";
        type Type = super::GenericDialog;
        type ParentType = gtk::Window;

        fn new() -> Self {
            let title = title("");

            let cancel_button = gtk::Button::builder().label("Cancel").build();

            let ok_button = gtk::Button::builder()
                .label("OK")
                .receives_default(true)
                .build();
            ok_button.add_css_class("suggested-action");

            Self {
                title,
                cancel_button,
                ok_button,
                sender: Default::default(),
                receiver: Default::default(),
            }
        }
    }

    impl ObjectImpl for GenericDialog {
        fn constructed(&self) {
            self.parent_constructed();

            let header = gtk::HeaderBar::builder()
                .title_widget(&self.title)
                .show_title_buttons(false)
                .build();

            let (sender, receiver) = channel::<gtk::ResponseType>(0);
            *self.sender.borrow_mut() = Some(sender.clone());
            *self.receiver.borrow_mut() = Some(receiver);

            self.cancel_button.connect_clicked(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_| imp.send(gtk::ResponseType::Cancel)
            ));
            header.pack_start(&self.cancel_button);

            self.ok_button.connect_clicked(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_| imp.send(gtk::ResponseType::Ok)
            ));
            header.pack_end(&self.ok_button);

            self.obj().set_modal(true);
            self.obj().set_resizable(true);
            self.obj().set_titlebar(Some(&header));
            self.obj().set_icon_name(Some("password-storage"));

            let key_controller = gtk::EventControllerKey::new();
            key_controller.connect_key_pressed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                glib::Propagation::Proceed,
                move |_controller, key, _keycode, modifier| {
                    const NO_MODIFIER: gdk::ModifierType = gdk::ModifierType::empty();
                    match (key, modifier) {
                        (gdk::Key::Escape, NO_MODIFIER)
                        | (gdk::Key::w, PRIMARY_MODIFIER)
                        | (gdk::Key::W, PRIMARY_MODIFIER) => {
                            imp.send(gtk::ResponseType::Cancel);
                            glib::Propagation::Stop
                        }
                        (gdk::Key::Return, NO_MODIFIER) => {
                            imp.ok_button.activate();
                            glib::Propagation::Stop
                        }
                        _ => glib::Propagation::Proceed,
                    }
                }
            ));
            self.obj().add_controller(key_controller);
        }
    }

    impl WidgetImpl for GenericDialog {}
    impl WindowImpl for GenericDialog {}

    impl GenericDialog {
        pub fn send(&self, response: gtk::ResponseType) {
            let mut sender_opt = self.sender.borrow_mut();
            let Some(sender) = sender_opt.as_mut() else {
                eprintln!("No sender");
                return;
            };
            if let Err(err) = sender.try_send(response) {
                eprintln!("Cannot send response {}. {}", response, err);
            }
        }

        pub async fn next_response(&self) -> Option<gtk::ResponseType> {
            self.receiver.borrow_mut().as_mut()?.next().await
        }
    }
}

glib::wrapper! {
  pub struct GenericDialog(ObjectSubclass<imp::GenericDialog>)
      @extends gtk::Widget, gtk::Window;
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

    pub fn emit_response(&self, response: gtk::ResponseType) {
        self.imp().send(response);
    }

    pub async fn run(&self) -> Option<gtk::ResponseType> {
        self.present();
        let result = self.imp().next_response().await;
        self.set_visible(false);
        result
    }
}
