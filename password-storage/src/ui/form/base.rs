use gtk::{Widget};

pub trait FormWidget<T> {
    fn get_widget(&self) -> Widget;
    fn get_value(&self) -> Option<T>;
    fn set_value(&self, value: Option<&T>);
    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&T>)>);
}
