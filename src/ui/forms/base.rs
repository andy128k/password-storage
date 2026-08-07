pub type ValueChangeCallback<T> = Box<dyn Fn(Option<&T>)>;

pub trait FormWidget<T> {
    fn get_widget(&self) -> gtk::Widget;
    fn get_value(&self) -> Option<T>;
    fn set_value(&self, value: Option<&T>);
    fn connect_changed(&mut self, callback: ValueChangeCallback<T>);
}
