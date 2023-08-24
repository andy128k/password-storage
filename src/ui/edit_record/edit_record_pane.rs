use crate::model::record::Record;
use crate::ui::edit_record::record_widget::RecordWidget;
use futures::channel::mpsc::{Receiver, Sender, channel};
use futures::stream::StreamExt;
use gtk::{gdk, glib, prelude::*, subclass::prelude::*};
use std::cell::RefCell;

mod imp {
    use super::*;
    use crate::utils::ui::hexpander;
    use awesome_gtk::widget::AwesomeWidgetTraverseExt;

    pub struct EditRecordPane {
        pub grid: gtk::Grid,
        pub save_button: gtk::Button,
        pub cancel_button: gtk::Button,
        pub key_controller: gtk::EventControllerKey,
        pub receiver: RefCell<Option<Receiver<gtk::ResponseType>>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for EditRecordPane {
        const NAME: &'static str = "PSEditRecordPane";
        type Type = super::EditRecordPane;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            let grid = gtk::Grid::builder()
                .margin_start(10)
                .margin_end(10)
                .margin_top(10)
                .margin_bottom(10)
                .row_spacing(10)
                .column_spacing(10)
                .column_homogeneous(false)
                .build();

            let save_button = gtk::Button::builder()
                .label("_Save")
                .use_underline(true)
                .build();
            save_button.add_css_class("suggested-action");

            let cancel_button = gtk::Button::builder()
                .label("_Cancel")
                .use_underline(true)
                .build();

            Self {
                grid,
                save_button,
                cancel_button,
                key_controller: Default::default(),
                receiver: Default::default(),
            }
        }
    }

    impl ObjectImpl for EditRecordPane {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            self.grid.attach(&self.cancel_button, 0, 1, 1, 1);
            self.grid.attach(&hexpander(), 1, 1, 1, 1);
            self.grid.attach(&self.save_button, 2, 1, 1, 1);
            self.grid.set_parent(&*obj);

            let (sender, receiver) = channel::<gtk::ResponseType>(0);
            *self.receiver.borrow_mut() = Some(receiver);

            self.cancel_button.connect_clicked({
                let sender = ResponseSender::new(&sender);
                move |_| sender.send(gtk::ResponseType::Cancel)
            });
            self.save_button.connect_clicked({
                let sender = ResponseSender::new(&sender);
                move |_| sender.send(gtk::ResponseType::Accept)
            });
            obj.add_controller(self.key_controller.clone());
            self.key_controller.connect_key_pressed({
                let sender = ResponseSender::new(&sender);
                move |_, key, _keycode, _modifier| {
                    if key == gdk::Key::Escape {
                        sender.send(gtk::ResponseType::Cancel);
                    }
                    glib::Propagation::Proceed
                }
            });
        }

        fn dispose(&self) {
            for child in self.obj().children() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for EditRecordPane {}

    impl EditRecordPane {
        pub async fn next_response(&self) -> Option<gtk::ResponseType> {
            let response = self.receiver.borrow_mut().as_mut()?.next().await?;
            Some(response)
        }
    }
}

glib::wrapper! {
    pub struct EditRecordPane(ObjectSubclass<imp::EditRecordPane>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for EditRecordPane {
    fn default() -> Self {
        glib::Object::builder().build()
    }
}

impl EditRecordPane {
    pub async fn run(&self, record: &Record, names: Vec<String>) -> Option<Record> {
        self.set_visible(true);

        let editor = RecordWidget::new(names);
        editor.connect_record_changed(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |editor| {
                this.imp()
                    .save_button
                    .set_sensitive(editor.record().is_some());
            }
        ));
        editor.set_record(record);
        self.imp()
            .save_button
            .set_sensitive(editor.record().is_some());

        self.imp().grid.attach(&editor, 0, 0, 3, 1);
        editor.grab_focus_to_editor();

        let button = self.imp().next_response().await?;
        editor.unparent();
        if button != gtk::ResponseType::Accept {
            None
        } else {
            editor.record()
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
