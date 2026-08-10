use super::base::*;
use crate::ui::error_label::create_error_label;
use gtk::{glib, prelude::*, subclass::prelude::*};
use std::cell::RefCell;

pub type FormData = Vec<String>;
pub type FormDataChanged = ValueChangeCallback<FormData>;

pub enum ValidationResult {
    Valid,
    Invalid(String),
}

pub type FormDataValidate = Box<dyn Fn(&FormData) -> ValidationResult>;

struct FormEntry {
    widget: Box<dyn FormWidget<String>>,
    required: bool,
}

struct FormValidation {
    label: gtk::Label,
    callback: FormDataValidate,
}

mod imp {
    use super::*;
    use crate::utils::ui::orphan_all_children;

    pub struct Form {
        pub(super) grid: gtk::Grid,
        pub(super) entries: RefCell<Vec<FormEntry>>,
        pub(super) change_callback: RefCell<Option<FormDataChanged>>,
        pub(super) validation: RefCell<Option<FormValidation>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for Form {
        const NAME: &'static str = "PSForm";
        type Type = super::Form;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            Self {
                grid: gtk::Grid::builder()
                    .column_spacing(10)
                    .row_spacing(10)
                    .build(),
                entries: Default::default(),
                change_callback: Default::default(),
                validation: Default::default(),
            }
        }
    }

    impl ObjectImpl for Form {
        fn constructed(&self) {
            self.parent_constructed();

            let form = self.obj();
            form.set_layout_manager(Some(gtk::BinLayout::new()));

            self.grid.set_parent(&*form);
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
        }
    }

    impl WidgetImpl for Form {}

    impl Form {
        pub(super) fn get_value(&self) -> Option<FormData> {
            let mut new_entry = Vec::new();
            for FormEntry {
                widget, required, ..
            } in self.entries.borrow().iter()
            {
                match widget.get_value() {
                    Some(value) => new_entry.push(value),
                    None if *required => return None,
                    None => new_entry.push(String::new()),
                }
            }
            Some(new_entry)
        }

        pub(super) fn set_value(&self, value: Option<&FormData>) {
            match value {
                Some(entry) => {
                    for (FormEntry { widget, .. }, value) in
                        self.entries.borrow().iter().zip(entry.iter())
                    {
                        widget.set_value(Some(value));
                    }
                }
                None => {
                    for FormEntry { widget, .. } in self.entries.borrow().iter() {
                        widget.set_value(None);
                    }
                }
            }
        }

        pub(super) fn field_changed(&self) {
            let value = self.get_value();
            if let Some(validation) = self.validation.borrow().as_ref() {
                if let Some(ref raw_value) = value {
                    match (validation.callback)(raw_value) {
                        ValidationResult::Valid => {
                            validation.label.set_text("");
                            validation.label.set_visible(false);
                            self.notify_change(value.as_ref());
                        }
                        ValidationResult::Invalid(message) => {
                            validation.label.set_text(&message);
                            validation.label.set_visible(true);
                            self.notify_change(None);
                        }
                    };
                } else {
                    validation.label.set_visible(false);
                    validation.label.set_text("");
                    self.notify_change(None);
                }
            } else {
                self.notify_change(value.as_ref());
            }
        }

        fn notify_change(&self, value: Option<&FormData>) {
            if let Some(cb) = self.change_callback.borrow().as_ref() {
                cb(value);
            }
        }
    }
}

glib::wrapper! {
    pub struct Form(ObjectSubclass<imp::Form>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for Form {
    fn default() -> Self {
        glib::Object::new()
    }
}

impl Form {
    pub fn add(&self, label: &str, mut widget: Box<dyn FormWidget<String>>, required: bool) {
        let index = self.imp().entries.borrow().len();

        let label_widget = gtk::Label::new(Some(label));
        label_widget.set_xalign(0.0_f32);
        label_widget.set_yalign(0.5_f32);
        self.imp().grid.attach(&label_widget, 0, index as i32, 1, 1);

        widget.connect_changed(Box::new(glib::clone!(
            #[weak(rename_to = this)]
            self,
            move |_field_value| {
                this.imp().field_changed();
            }
        )));
        self.imp()
            .grid
            .attach(&widget.get_widget(), 1, index as i32, 1, 1);

        self.imp()
            .entries
            .borrow_mut()
            .push(FormEntry { widget, required });
    }

    pub fn set_validator(&self, validate: FormDataValidate) {
        let index = self.imp().entries.borrow().len();
        let error_label = create_error_label();
        self.imp().grid.attach(&error_label, 0, index as i32, 2, 1);

        self.imp().validation.replace(Some(FormValidation {
            label: error_label,
            callback: validate,
        }));
    }
}

impl FormWidget<FormData> for Form {
    fn get_widget(&self) -> gtk::Widget {
        self.clone().upcast()
    }

    fn get_value(&self) -> Option<FormData> {
        self.imp().get_value()
    }

    fn set_value(&self, value: Option<&FormData>) {
        self.imp().set_value(value);
    }

    fn connect_changed(&mut self, callback: FormDataChanged) {
        self.imp().change_callback.replace(Some(callback));
    }
}
