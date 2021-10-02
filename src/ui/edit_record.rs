use super::edit_object::edit_object;
use super::forms::base::*;
use super::forms::entry::*;
use super::forms::form::*;
use super::forms::multiline::*;
use super::password_editor::PasswordEditor;
use crate::gtk_prelude::*;
use crate::model::record::FIELD_NAME;
use crate::model::record::RECORD_TYPES;
use crate::model::record::{FieldType, Record, RecordType};
use crate::ui::record_type_popover::record_type_popover;
use std::cell::RefCell;
use std::rc::Rc;

fn record_to_vec(record_type: &'static RecordType, record: &Record) -> Vec<String> {
    let mut values = Vec::new();
    for field in &record_type.fields {
        values.push(record.get_field(field).to_string());
    }
    values
}

fn vec_to_record(record_type: &'static RecordType, values: &[String]) -> Record {
    let mut record = record_type.new_record();
    for (field, value) in record_type.fields.iter().zip(values.iter()) {
        record.set_field(field, value);
    }
    record
}

struct RecordForm {
    record_type: &'static RecordType,
    form: Form,
}

impl RecordForm {
    fn new(record_type: &'static RecordType, names: &[String]) -> Self {
        let mut form = Form::new();
        for field in &record_type.fields {
            let fw: Box<dyn FormWidget<String>> = match field.field_type {
                FieldType::Text => Box::new(Text::new()),
                FieldType::MultiLine => Box::new(MultiLine::new()),
                FieldType::Name => Box::new(Name::new(names)),
                FieldType::Password => Box::new(PasswordEditor::new()),
                FieldType::Secret => Box::new(Text::new()),
            };
            let required = field.name == "name";
            form.add(field.title, fw, required);
        }
        Self { record_type, form }
    }
}

impl FormWidget<Record> for RecordForm {
    fn get_widget(&self) -> gtk::Widget {
        self.form.get_widget()
    }

    fn get_value(&self) -> Option<Record> {
        self.form
            .get_value()
            .map(|vec| vec_to_record(self.record_type, &vec))
    }

    fn set_value(&self, value: Option<&Record>) {
        self.form.set_value(
            value
                .map(|record| record_to_vec(self.record_type, record))
                .as_ref(),
        );
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&Record>)>) {
        let record_type = self.record_type;
        self.form.connect_changed(Box::new(move |values| {
            let record = values.map(|vec| vec_to_record(record_type, vec));
            callback(record.as_ref());
        }));
    }
}

#[derive(Clone, glib::Downgrade)]
pub struct RecordWidget(Rc<RecordWidgetPrivate>);

pub struct RecordWidgetPrivate {
    grid: gtk::Grid,
    icon: gtk::Image,
    type_label: gtk::Label,
    convert_button: gtk::MenuButton,
    form: RefCell<Option<RecordForm>>,
    names: Vec<String>,
    callback: RefCell<Box<dyn Fn(Option<&Record>)>>,
}

fn no_op(_: Option<&Record>) {}

impl RecordWidget {
    fn new(names: Vec<String>) -> Self {
        let grid = gtk::Grid::builder()
            .column_spacing(10)
            .row_spacing(10)
            .row_homogeneous(false)
            .column_homogeneous(false)
            .build();

        let icon = gtk::Image::builder()
            .icon_size(gtk::IconSize::Dialog)
            .margin_start(16)
            .margin_end(16)
            .margin_top(16)
            .margin_bottom(8)
            .halign(gtk::Align::Center)
            .vexpand(false)
            .build();
        grid.attach(&icon, 0, 0, 1, 1);

        let type_label = gtk::Label::builder().vexpand(false).xalign(0.5_f32).build();
        grid.attach(&type_label, 0, 1, 1, 1);

        let convert_button = gtk::MenuButton::builder()
            .label("Convert to...")
            .no_show_all(true)
            .build();
        grid.attach(&convert_button, 0, 2, 1, 1);

        let expander = gtk::Label::builder().vexpand(true).build();
        grid.attach(&expander, 0, 3, 1, 1);

        let separator = gtk::Separator::builder()
            .orientation(gtk::Orientation::Vertical)
            .build();
        grid.attach(&separator, 1, 0, 1, 4);

        let widget = Self(Rc::new(RecordWidgetPrivate {
            grid,
            icon,
            type_label,
            convert_button,
            form: RefCell::new(None),
            names,
            callback: RefCell::new(Box::new(no_op)),
        }));
        widget
    }

    fn set_record_type(&self, record_type: &'static RecordType) {
        if self
            .get_record_type()
            .map_or(false, |rt| rt.ref_eq(record_type))
        {
            return;
        }

        self.0.icon.set_icon_name(Some(record_type.icon));
        self.0.type_label.set_label(record_type.title);
        if !record_type.is_group {
            let convert_to_types = RECORD_TYPES
                .iter()
                .filter(|rt| !rt.is_group && !rt.ref_eq(record_type))
                .cloned()
                .collect::<Vec<_>>();

            let popover = record_type_popover(
                &convert_to_types,
                glib::clone!(@weak self as this => move |dest_record_type| {
                    this.convert_to(dest_record_type);
                }),
            );
            self.0.convert_button.set_popover(Some(&popover));
            self.0.convert_button.show();
        } else {
            self.0.convert_button.hide();
        }

        if let Some(old_form) = self.0.grid.child_at(2, 0) {
            self.0.grid.remove(&old_form);
        }
        let mut form = RecordForm::new(record_type, &self.0.names);
        form.connect_changed(Box::new(glib::clone!(@weak self as this => move |value| {
            this.0.callback.borrow()(value);
        })));
        let form_widget = form.get_widget();
        *self.0.form.borrow_mut() = Some(form);

        self.0.grid.attach(&form_widget, 2, 0, 1, 4);
        self.0.grid.show_all();

        self.0.grid.set_focus_child(Some(&form_widget));
    }

    fn get_record_type(&self) -> Option<&'static RecordType> {
        self.0.form.borrow().as_ref().map(|form| form.record_type)
    }

    fn convert_to(&self, dest_record_type: &'static RecordType) {
        let mut new_record = dest_record_type.new_record();
        if let Some(record) = self.get_value() {
            let name = record.get_field(&FIELD_NAME);
            new_record.set_field(&FIELD_NAME, name);
            new_record.join(&record);
        }
        self.set_value(Some(&new_record));
    }
}

impl FormWidget<Record> for RecordWidget {
    fn get_widget(&self) -> gtk::Widget {
        self.0.grid.clone().upcast()
    }

    fn get_value(&self) -> Option<Record> {
        self.0.form.borrow().as_ref().and_then(|f| f.get_value())
    }

    fn set_value(&self, value: Option<&Record>) {
        if let Some(value) = value {
            self.set_record_type(value.record_type);
        }
        self.0.form.borrow().as_ref().unwrap().set_value(value);
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&Record>)>) {
        *self.0.callback.borrow_mut() = callback;
    }
}

pub async fn edit_record(
    record: &Record,
    parent_window: &gtk::Window,
    title: &str,
    names: Vec<String>,
) -> Option<Record> {
    edit_object(
        Some(record),
        RecordWidget::new(names),
        parent_window,
        title,
        "password-storage",
    )
    .await
}
