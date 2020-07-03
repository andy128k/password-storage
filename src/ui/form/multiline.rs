use super::base::*;
use crate::utils::string::non_empty;
use gtk::prelude::*;
use gtk::{Adjustment, PolicyType, ScrolledWindow, ShadowType, TextBuffer, TextView, Widget};

pub struct MultiLine {
    scrolled_window: ScrolledWindow,
    text_view: TextView,
}

impl MultiLine {
    pub fn new() -> Self {
        let text_view = TextView::new();
        text_view.set_can_focus(true);
        text_view.set_accepts_tab(false);
        text_view.set_left_margin(8);
        text_view.set_right_margin(8);
        text_view.set_top_margin(8);
        text_view.set_bottom_margin(8);

        let scrolled_window = ScrolledWindow::new(None::<&Adjustment>, None::<&Adjustment>);
        scrolled_window.set_can_focus(true);
        scrolled_window.set_property_hscrollbar_policy(PolicyType::Automatic);
        scrolled_window.set_property_vscrollbar_policy(PolicyType::Automatic);
        scrolled_window.set_shadow_type(ShadowType::In);
        scrolled_window.add(&text_view);

        scrolled_window.set_hexpand(true);
        scrolled_window.set_vexpand(true);
        scrolled_window.set_size_request(300, 200);

        MultiLine {
            scrolled_window,
            text_view,
        }
    }
}

fn buffer_get_text(buffer: &TextBuffer) -> Option<String> {
    let (start, end) = buffer.get_bounds();
    buffer.get_text(&start, &end, true).and_then(non_empty)
}

impl FormWidget<String> for MultiLine {
    fn get_widget(&self) -> Widget {
        self.scrolled_window.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        self.text_view
            .get_buffer()
            .and_then(|buffer| buffer_get_text(&buffer))
    }

    fn set_value(&self, value: Option<&String>) {
        if let Some(buffer) = self.text_view.get_buffer() {
            match value {
                Some(text) => buffer.set_text(text),
                None => buffer.set_text(""),
            }
        }
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        if let Some(buffer) = self.text_view.get_buffer() {
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
