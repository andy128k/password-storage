use super::edit_object::edit_object;
use super::forms::base::*;
use super::forms::entry::*;
use super::forms::form::*;
use super::forms::multiline::*;
use super::password_editor::PasswordEditor;
use crate::gtk_prelude::*;
use crate::model::record::{FieldType, Record, RecordType};

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

pub async fn edit_record(
    record: &Record,
    parent_window: &gtk::Window,
    title: &str,
    names: &[String],
) -> Option<Record> {
    edit_object(
        Some(record),
        RecordForm::new(record.record_type, names),
        parent_window,
        &format!("{} {}", title, record.record_type.title),
        record.record_type.icon,
    )
    .await
}
