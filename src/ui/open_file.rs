use crate::error::*;
use futures::channel::mpsc::{channel, Receiver, Sender};
use futures::stream::StreamExt;
use gtk::{gdk, glib, prelude::*, subclass::prelude::*};
use std::cell::RefCell;
use std::error::Error;

mod imp {
    use super::*;
    use crate::utils::ui::{hexpander, orphan_all_children, vexpander};

    pub struct OpenFile {
        pub entry: gtk::Entry,
        pub error_label: gtk::Label,
        pub open_button: gtk::Button,
        pub key_controller: gtk::EventControllerKey,
        pub receiver: RefCell<Option<Receiver<gtk::ResponseType>>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for OpenFile {
        const NAME: &'static str = "PSOpenFile";
        type Type = super::OpenFile;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            let error_label = gtk::Label::builder()
                .xalign(0.5)
                .yalign(1.0)
                .vexpand(true)
                .build();
            error_label.add_css_class("error");

            let open_button = gtk::Button::builder()
                .label("_Open file")
                .use_underline(true)
                .hexpand(true)
                .build();
            open_button.add_css_class("suggested-action");

            Self {
                entry: gtk::Entry::builder()
                    .can_focus(true)
                    .activates_default(true)
                    .visibility(false)
                    .hexpand(true)
                    .build(),
                error_label,
                open_button,
                key_controller: Default::default(),
                receiver: Default::default(),
            }
        }
    }

    impl ObjectImpl for OpenFile {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            let grid = gtk::Grid::builder()
                .width_request(400)
                .hexpand(true)
                .vexpand(true)
                .margin_start(8)
                .margin_end(8)
                .margin_top(8)
                .margin_bottom(8)
                .column_spacing(8)
                .row_spacing(8)
                .build();
            grid.set_parent(&*obj);

            let (sender, receiver) = channel::<gtk::ResponseType>(0);
            *self.receiver.borrow_mut() = Some(receiver);

            let label = gtk::Label::builder()
                .label("_Password")
                .use_underline(true)
                .mnemonic_widget(&self.entry)
                .xalign(0_f32)
                .yalign(0.5_f32)
                .build();

            let cancel_button = gtk::Button::builder()
                .label("_Cancel")
                .use_underline(true)
                .hexpand(true)
                .build();
            cancel_button.connect_clicked({
                let sender = ResponseSender::new(&sender);
                move |_| sender.send(gtk::ResponseType::Cancel)
            });

            self.open_button.connect_clicked({
                let sender = ResponseSender::new(&sender);
                move |_| sender.send(gtk::ResponseType::Accept)
            });

            self.entry.connect_changed(glib::clone!(
                #[weak(rename_to = open_button)]
                self.open_button,
                move |entry| {
                    open_button.set_sensitive(entry.text_length() > 0);
                }
            ));

            self.entry.connect_activate({
                let sender = ResponseSender::new(&sender);
                move |_| sender.send(gtk::ResponseType::Accept)
            });

            self.entry.add_controller(self.key_controller.clone());
            self.key_controller.connect_key_pressed({
                let sender = ResponseSender::new(&sender);
                move |_, key, _keycode, _modifier| {
                    if key == gdk::Key::Escape {
                        sender.send(gtk::ResponseType::Cancel);
                    }
                    glib::Propagation::Proceed
                }
            });
            let button_box = gtk::Box::builder()
                .orientation(gtk::Orientation::Horizontal)
                .homogeneous(true)
                // .hexpand(true)
                .halign(gtk::Align::Center)
                .spacing(8)
                .build();
            button_box.append(&cancel_button);
            button_box.append(&self.open_button);

            grid.attach(&self.error_label, 0, 0, 4, 1);

            grid.attach(&hexpander(), 0, 1, 1, 2);
            grid.attach(&hexpander(), 3, 1, 1, 2);

            grid.attach(&label, 1, 1, 1, 1);
            grid.attach(&self.entry, 2, 1, 1, 1);

            grid.attach(&button_box, 1, 2, 2, 1);

            grid.attach(&vexpander(), 0, 3, 3, 1);
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
        }
    }

    impl WidgetImpl for OpenFile {}

    impl OpenFile {
        pub fn reset(&self) {
            self.entry.set_text("");
            self.error_label.set_label("");
            self.open_button.set_sensitive(false);
        }

        pub fn set_error(&self, error: &dyn Error) {
            // self.error_label.set_visible(true);
            self.error_label.set_label(&format!("{}", error));
        }

        pub async fn next_response(&self) -> Option<gtk::ResponseType> {
            let response = self.receiver.borrow_mut().as_mut()?.next().await?;
            Some(response)
        }
    }
}

glib::wrapper! {
    pub struct OpenFile(ObjectSubclass<imp::OpenFile>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for OpenFile {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl OpenFile {
    pub async fn run<T, R>(&self, read_file_callback: R) -> Option<(T, String)>
    where
        T: 'static,
        R: Fn(&str) -> Result<T> + 'static,
    {
        self.set_visible(true);
        self.imp().reset();
        self.imp().entry.grab_focus();

        loop {
            let button = self.imp().next_response().await?;

            if button != gtk::ResponseType::Accept {
                self.imp().reset();
                return None;
            }

            let password = self.imp().entry.text();
            match read_file_callback(&password) {
                Ok(document) => {
                    self.imp().reset();
                    return Some((document, password.into()));
                }
                Err(e) => self.imp().set_error(&*e),
            }
        }
    }
}

struct ResponseSender(RefCell<Sender<gtk::ResponseType>>);

impl ResponseSender {
    fn new(sender: &Sender<gtk::ResponseType>) -> Self {
        Self(RefCell::new(sender.clone()))
    }

    fn send(&self, response: gtk::ResponseType) {
        if let Err(error) = self.0.borrow_mut().try_send(response) {
            eprintln!("{}", error);
        }
    }
}
