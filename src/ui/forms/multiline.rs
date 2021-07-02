use super::base::*;
use crate::utils::string::non_empty;
use gtk::prelude::*;

pub struct MultiLine {
    scrolled_window: gtk::ScrolledWindow,
    text_view: gtk::TextView,
}

impl MultiLine {
    pub fn new() -> Self {
        let text_view = gtk::TextView::builder()
            .can_focus(true)
            .accepts_tab(false)
            .left_margin(8)
            .right_margin(8)
            .top_margin(8)
            .bottom_margin(8)
            .build();

        let scrolled_window = gtk::ScrolledWindow::builder()
            .can_focus(true)
            .hscrollbar_policy(gtk::PolicyType::Automatic)
            .vscrollbar_policy(gtk::PolicyType::Automatic)
            .shadow_type(gtk::ShadowType::In)
            .hexpand(true)
            .vexpand(true)
            .width_request(300)
            .height_request(200)
            .build();
        scrolled_window.add(&text_view);

        Self {
            scrolled_window,
            text_view,
        }
    }
}

fn buffer_get_text(buffer: &gtk::TextBuffer) -> Option<String> {
    let (start, end) = buffer.bounds();
    buffer
        .text(&start, &end, true)
        .and_then(non_empty)
        .map(|gs| gs.to_string())
}

impl FormWidget<String> for MultiLine {
    fn get_widget(&self) -> gtk::Widget {
        self.scrolled_window.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        self.text_view
            .buffer()
            .and_then(|buffer| buffer_get_text(&buffer))
    }

    fn set_value(&self, value: Option<&String>) {
        if let Some(buffer) = self.text_view.buffer() {
            buffer.set_text(value.map(String::as_str).unwrap_or_default());
        }
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        if let Some(buffer) = self.text_view.buffer() {
            buffer.connect_changed(move |buffer| {
                callback(buffer_get_text(buffer).as_ref());
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::test_gtk_init;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_multiline() {
        test_gtk_init();

        let w = MultiLine::new();
        w.get_widget(); // ensure get_widget doesn't panic
    }

    #[test]
    fn test_multiline_value() {
        test_gtk_init();

        let w = MultiLine::new();
        assert_eq!(w.get_value(), None);

        let new_value = "new\nvalue".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(w.get_value(), Some(new_value));

        w.set_value(None);
        assert_eq!(w.get_value(), None);
    }

    #[test]
    fn test_multiline_event() {
        test_gtk_init();

        let value = Rc::new(RefCell::new(None));

        let mut w = MultiLine::new();
        let value2 = value.clone();
        w.connect_changed(Box::new(move |v| *value2.borrow_mut() = v.clone().cloned()));

        let new_value = "new\nvalue".to_string();
        w.set_value(Some(&new_value));
        assert_eq!(*value.borrow(), Some(new_value));
    }
}
