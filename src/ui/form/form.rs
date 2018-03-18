use std::rc::Rc;
use std::cell::RefCell;
use gtk::prelude::*;
use gtk::{Grid, Label, Widget};
use ui::error_label::create_error_label;
use super::base::*;

pub type FormData = Vec<String>;
pub type FormDataChanged = Box<Fn(Option<&FormData>)>;

pub enum ValidationResult {
    Valid,
    Invalid(String)
}

pub type FormDataValidate = Box<Fn(&FormData) -> ValidationResult>;

struct FormEntry {
    label: String,
    widget: Box<FormWidget<String>>,
    required: bool,
}

struct FormValidation {
    label: Label,
    callback: FormDataValidate
}

struct FormPrivate {
    grid: Grid,
    fields: Vec<FormEntry>,
    change_callback: Option<FormDataChanged>,
    validation: Option<FormValidation>
}

impl FormPrivate {
    fn get_value(&self) -> Option<FormData> {
        let mut new_entry = Vec::new();
        for &FormEntry { ref widget, required, .. } in &self.fields {
            match widget.get_value() {
                Some(value) => new_entry.push(value),
                None if required => return None,
                None => new_entry.push(String::new())
            };
        }
        Some(new_entry)
    }

    fn set_value(&self, value: Option<&FormData>) {
        match value {
            Some(entry) => {
                for (&FormEntry { ref widget, .. }, value) in self.fields.iter().zip(entry.iter()) {
                    widget.set_value(Some(value));
                }
            },
            None => {
                for &FormEntry { ref widget, .. } in &self.fields {
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
                    },
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
        self.change_callback.as_ref().map(|cb| cb(value));
    }
}

pub struct Form(Rc<RefCell<FormPrivate>>);

impl Form {
    pub fn new() -> Self {
        let grid = Grid::new();
        grid.set_column_spacing(8);
        grid.set_row_spacing(8);

        let private = FormPrivate {
            grid: grid.clone(),
            fields: Vec::new(),
            change_callback: None,
            validation: None
        };

        let private_ref = Rc::new(RefCell::new(private));

        Form(private_ref)
    }

    pub fn add(&mut self, label: &str, mut widget: Box<FormWidget<String>>, required: bool) {
        let index = self.0.borrow().fields.len();

        let label_widget = Label::new(label);
        label_widget.set_xalign(0f32);
        label_widget.set_yalign(0.5f32);
        self.0.borrow().grid.attach(&label_widget, 0, index as i32, 1, 1);

        let private_ref_c = self.0.clone();
        widget.connect_changed(Box::new(move |_field_value| {
            private_ref_c.borrow().field_changed();
        }));
        self.0.borrow().grid.attach(&widget.get_widget(), 1, index as i32, 1, 1);

        self.0.borrow_mut().fields.push(FormEntry { label: label.to_string(), widget, required });
    }

    pub fn set_validator(&mut self, validate: FormDataValidate) {
        let mut private = self.0.borrow_mut();

        let index = private.fields.len();
        let error_label = create_error_label().expect("Error label is created.");
        private.grid.attach(&error_label, 0, index as i32, 2, 1);

        (*private).validation = Some(FormValidation { label: error_label, callback: validate });
    }
}

impl FormWidget<FormData> for Form {
    fn get_widget(&self) -> Widget {
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
        (*private).change_callback = Some(callback);
    }
}
