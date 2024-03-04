use crate::model::record::{Field, FieldType, Record, RecordType};
use gtk::{glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::ui::forms::base::*;
    use crate::ui::forms::entry::*;
    use crate::ui::forms::multiline::*;
    use crate::ui::password_editor::PasswordEditor;
    use crate::utils::ui::orphan_all_children;
    use std::cell::OnceCell;
    use std::sync::OnceLock;

    struct FormEntry {
        field: &'static Field,
        widget: Box<dyn FormWidget<String>>,
    }

    pub struct RecordForm {
        grid: gtk::Grid,
        pub record_type: OnceCell<&'static RecordType>,
        entries: OnceCell<Vec<FormEntry>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for RecordForm {
        const NAME: &'static str = "PSRecordForm";
        type Type = super::RecordForm;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            Self {
                grid: gtk::Grid::builder()
                    .column_spacing(10)
                    .row_spacing(10)
                    .build(),
                record_type: Default::default(),
                entries: Default::default(),
            }
        }
    }

    impl ObjectImpl for RecordForm {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            self.grid.set_parent(&*obj);
        }

        fn signals() -> &'static [glib::subclass::Signal] {
            static SIGNALS: OnceLock<Vec<glib::subclass::Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| vec![glib::subclass::Signal::builder("record-changed").build()])
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
        }
    }

    impl WidgetImpl for RecordForm {}

    impl RecordForm {
        pub fn init(&self, record_type: &'static RecordType, names: &[String]) {
            self.record_type.set(record_type).unwrap();

            let mut entries: Vec<FormEntry> = Vec::new();
            for (index, field) in record_type.fields.iter().enumerate() {
                let mut widget: Box<dyn FormWidget<String>> = match field.field_type {
                    FieldType::Text => Box::new(form_entry()),
                    FieldType::MultiLine => Box::new(MultiLine::new()),
                    FieldType::Name => Box::new(form_entry_with_completion(names)),
                    FieldType::Password => Box::new(PasswordEditor::new()),
                    FieldType::Secret => Box::new(form_password_entry()),
                };

                let label_widget = gtk::Label::builder()
                    .label(field.title)
                    .xalign(0_f32)
                    .yalign(0.5_f32)
                    .build();
                self.grid.attach(&label_widget, 0, index as i32, 1, 1);

                widget.connect_changed(Box::new(glib::clone!(@weak self as this => move |_| {
                    this.obj().emit_by_name::<()>("record-changed", &[]);
                })));
                self.grid
                    .attach(&widget.get_widget(), 1, index as i32, 1, 1);

                entries.push(FormEntry { field, widget });
            }
            self.entries.set(entries).ok().unwrap();
        }

        pub fn record(&self) -> Record {
            let mut record = self.record_type.get().unwrap().new_record();
            for &FormEntry { field, ref widget } in self.entries.get().unwrap().iter() {
                if let Some(value) = widget.get_value() {
                    record.set_field(field, &value);
                }
            }
            record
        }

        pub fn set_record(&self, record: &Record) {
            for &FormEntry { field, ref widget } in self.entries.get().unwrap().iter() {
                widget.set_value(Some(&record.get_field(field).to_string()));
            }
        }

        pub fn grab_focus_to_editor(&self) {
            if let Some(first) = self.entries.get().unwrap().first() {
                first.widget.get_widget().grab_focus();
            }
        }
    }
}

glib::wrapper! {
    pub struct RecordForm(ObjectSubclass<imp::RecordForm>)
        @extends gtk::Widget;
}

impl RecordForm {
    pub fn new(record_type: &'static RecordType, names: &[String]) -> Self {
        let obj: Self = glib::Object::builder().build();
        obj.imp().init(record_type, names);
        obj
    }

    pub fn record_type(&self) -> &'static RecordType {
        self.imp().record_type.get().unwrap()
    }

    pub fn record(&self) -> Record {
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
