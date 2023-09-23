use super::record_form::RecordForm;
use crate::model::record::{Record, RecordType, FIELD_NAME, RECORD_TYPES};
use crate::ui::record_type_popover::RecordTypePopoverBuilder;
use gtk::{glib, prelude::*, subclass::prelude::*};
use std::cell::RefCell;

mod imp {
    use super::*;
    use std::sync::OnceLock;

    pub struct RecordWidget {
        grid: gtk::Grid,
        icon: gtk::Image,
        type_label: gtk::Label,
        convert_button: gtk::MenuButton,
        open_button: gtk::Button,
        form: RefCell<Option<RecordForm>>,
        pub names: RefCell<Vec<String>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for RecordWidget {
        const NAME: &'static str = "PSRecordWidget";
        type Type = super::RecordWidget;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            Self {
                grid: gtk::Grid::builder()
                    .column_spacing(10)
                    .row_spacing(10)
                    .row_homogeneous(false)
                    .column_homogeneous(false)
                    .build(),
                icon: gtk::Image::builder()
                    .icon_size(gtk::IconSize::Large)
                    .margin_start(16)
                    .margin_end(16)
                    .margin_top(16)
                    .margin_bottom(8)
                    .halign(gtk::Align::Center)
                    .vexpand(false)
                    .build(),
                type_label: gtk::Label::builder().vexpand(false).xalign(0.5_f32).build(),
                convert_button: gtk::MenuButton::builder()
                    .label("Convert to...")
                    .visible(false)
                    .build(),
                open_button: gtk::Button::builder().label("Open").visible(false).build(),
                form: Default::default(),
                names: Default::default(),
            }
        }
    }

    impl ObjectImpl for RecordWidget {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));
            obj.set_hexpand(true);
            obj.set_vexpand(true);

            self.grid.set_parent(&*obj);

            self.grid.attach(&self.icon, 0, 0, 1, 1);
            self.grid.attach(&self.type_label, 0, 1, 1, 1);
            self.grid.attach(&self.convert_button, 0, 2, 1, 1);
            self.grid.attach(&self.open_button, 0, 3, 1, 1);

            let expander = gtk::Label::builder().vexpand(true).build();
            self.grid.attach(&expander, 0, 4, 1, 1);

            let separator = gtk::Separator::builder()
                .orientation(gtk::Orientation::Vertical)
                .build();
            self.grid.attach(&separator, 1, 0, 1, 5);

            self.open_button
                .connect_clicked(glib::clone!(@weak self as imp => move |_| imp.open()));
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| vec![glib::subclass::Signal::builder("record-changed").build()])
        }

        fn dispose(&self) {
            while let Some(child) = self.obj().first_child() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for RecordWidget {}

    impl RecordWidget {
        fn record_type(&self) -> Option<&'static RecordType> {
            self.form.borrow().as_ref().map(|form| form.record_type())
        }

        fn set_record_type(&self, record_type: &'static RecordType) {
            if self.record_type() == Some(record_type) {
                return;
            }

            self.icon.set_icon_name(Some(record_type.icon));
            self.type_label.set_label(record_type.title);
            if !record_type.is_group {
                let convert_to_types = RECORD_TYPES
                    .iter()
                    .filter(|rt| !rt.is_group && **rt != record_type)
                    .cloned()
                    .collect::<Vec<_>>();

                let popover = RecordTypePopoverBuilder::default()
                    .record_types(&convert_to_types)
                    .on_activate(glib::clone!(@weak self as this => move |dest_record_type| {
                        this.convert_to(dest_record_type);
                    }))
                    .build();

                self.convert_button.set_popover(Some(&popover));
                self.convert_button.show();
            } else {
                self.convert_button.hide();
            }

            if let Some(old_form) = self.grid.child_at(2, 0) {
                self.grid.remove(&old_form);
            }
            let form = RecordForm::new(record_type, &self.names.borrow());
            form.connect_record_changed(glib::clone!(@weak self as this => move |_| {
                this.obj().emit_by_name::<()>("record-changed", &[]);
            }));
            *self.form.borrow_mut() = Some(form.clone());

            form.set_hexpand(true);
            form.set_vexpand(true);
            self.grid.attach(&form, 2, 0, 1, 5);

            self.grid.set_focus_child(Some(&form));
        }

        fn raw_record(&self) -> Option<Record> {
            self.form.borrow().as_ref().map(|f| f.record())
        }

        pub fn record(&self) -> Option<Record> {
            self.raw_record().filter(|r| !r.name().is_empty())
        }

        pub fn set_record(&self, record: &Record) {
            self.set_record_type(record.record_type);

            let has_url = record.url().is_some();
            self.open_button.set_visible(has_url);

            self.form.borrow().as_ref().unwrap().set_record(record);
        }

        fn open(&self) {
            let Some(record) = self.raw_record() else {
                return;
            };
            let Some(url) = record.url() else { return };
            let window = self.obj().root().and_downcast::<gtk::Window>();
            gtk::show_uri(window.as_ref(), url, 0);
        }

        fn convert_to(&self, dest_record_type: &'static RecordType) {
            let mut new_record = dest_record_type.new_record();
            if let Some(record) = self.raw_record() {
                let name = record.get_field(&FIELD_NAME);
                new_record.set_field(&FIELD_NAME, name);
                new_record.join(&record);
            }
            self.set_record(&new_record);
        }

        pub fn grab_focus_to_editor(&self) {
            self.form
                .borrow()
                .as_ref()
                .map(|w| w.grab_focus_to_editor());
        }
    }
}

glib::wrapper! {
    pub struct RecordWidget(ObjectSubclass<imp::RecordWidget>)
        @extends gtk::Widget;
}

impl RecordWidget {
    pub fn new(names: Vec<String>) -> Self {
        let obj: Self = glib::Object::builder().build();
        *obj.imp().names.borrow_mut() = names;
        obj
    }

    pub fn record(&self) -> Option<Record> {
        self.imp().record()
    }

    pub fn set_record(&self, record: &Record) {
        self.imp().set_record(record);
    }

    pub fn grab_focus_to_editor(&self) {
        self.imp().grab_focus_to_editor();
    }

    pub fn connect_record_changed<F>(&self, f: F) -> glib::signal::SignalHandlerId
    where
        F: Fn(&Self) + 'static,
    {
        self.connect_closure(
            "record-changed",
            false,
            glib::closure_local!(move |self_: &Self| (f)(self_)),
        )
    }
}
