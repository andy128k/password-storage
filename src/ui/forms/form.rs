use super::base::*;
use crate::ui::error_label::create_error_label;
use gtk::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;

pub type FormData = Vec<String>;
pub type FormDataChanged = Box<dyn Fn(Option<&FormData>)>;

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

struct FormPrivate {
    grid: gtk::Grid,
    entries: Vec<FormEntry>,
    change_callback: Option<FormDataChanged>,
    validation: Option<FormValidation>,
}

impl FormPrivate {
    fn get_value(&self) -> Option<FormData> {
        let mut new_entry = Vec::new();
        for &FormEntry {
            ref widget,
            required,
            ..
        } in &self.entries
        {
            match widget.get_value() {
                Some(value) => new_entry.push(value),
                None if required => return None,
                None => new_entry.push(String::new()),
            };
        }
        Some(new_entry)
    }

    fn set_value(&self, value: Option<&FormData>) {
        match value {
            Some(entry) => {
                for (FormEntry { widget, .. }, value) in self.entries.iter().zip(entry.iter()) {
                    widget.set_value(Some(value));
                }
            }
            None => {
                for FormEntry { widget, .. } in &self.entries {
                    widget.set_value(None);
                }
            }
        }
    }

    fn field_changed(&self) {
        let value = self.get_value();
        if let Some(ref validation) = self.validation {
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
        if let Some(ref cb) = self.change_callback {
            cb(value);
        }
    }
}

pub struct Form(Rc<RefCell<FormPrivate>>);

impl Form {
    pub fn new() -> Self {
        let grid = gtk::Grid::builder()
            .column_spacing(10)
            .row_spacing(10)
            .build();

        let private = FormPrivate {
            grid,
            entries: Vec::new(),
            change_callback: None,
            validation: None,
        };

        let private_ref = Rc::new(RefCell::new(private));

        Form(private_ref)
    }

    pub fn add(&mut self, label: &str, mut widget: Box<dyn FormWidget<String>>, required: bool) {
        let index = self.0.borrow().entries.len();

        let label_widget = gtk::Label::new(Some(label));
        label_widget.set_xalign(0f32);
        label_widget.set_yalign(0.5f32);
        self.0
            .borrow()
            .grid
            .attach(&label_widget, 0, index as i32, 1, 1);

        let private_ref_c = self.0.clone();
        widget.connect_changed(Box::new(move |_field_value| {
            private_ref_c.borrow().field_changed();
        }));
        self.0
            .borrow()
            .grid
            .attach(&widget.get_widget(), 1, index as i32, 1, 1);

        self.0
            .borrow_mut()
            .entries
            .push(FormEntry { widget, required });
    }

    pub fn set_validator(&mut self, validate: FormDataValidate) {
        let mut private = self.0.borrow_mut();

        let index = private.entries.len();
        let error_label = create_error_label();
        private.grid.attach(&error_label, 0, index as i32, 2, 1);

        private.validation = Some(FormValidation {
            label: error_label,
            callback: validate,
        });
    }
}

impl FormWidget<FormData> for Form {
    fn get_widget(&self) -> gtk::Widget {
        self.0.borrow().grid.clone().upcast()
    }

    fn get_value(&self) -> Option<FormData> {
        self.0.borrow().get_value()
    }

    fn set_value(&self, value: Option<&FormData>) {
        self.0.borrow().set_value(value);
    }

    fn connect_changed(&mut self, callback: FormDataChanged) {
        let mut private = self.0.borrow_mut();
        private.change_callback = Some(callback);
    }
}
