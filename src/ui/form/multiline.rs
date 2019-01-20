use super::base::*;
use gtk::prelude::*;
use gtk::{Widget, TextView, TextBuffer, ScrolledWindow, PolicyType, ShadowType};
use crate::utils::string::non_empty;

pub struct MultiLine {
    scrolled_window: ScrolledWindow,
    text_view: TextView
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

        let scrolled_window = ScrolledWindow::new(None, None);
        scrolled_window.set_can_focus(true);
        scrolled_window.set_property_hscrollbar_policy(PolicyType::Automatic);
        scrolled_window.set_property_vscrollbar_policy(PolicyType::Automatic);
        scrolled_window.set_shadow_type(ShadowType::In);
        scrolled_window.add(&text_view);

        scrolled_window.set_hexpand(true);
        scrolled_window.set_vexpand(true);

        MultiLine { scrolled_window, text_view }
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
        self.text_view.get_buffer()
            .and_then(|buffer| buffer_get_text(&buffer))
    }

    fn set_value(&self, value: Option<&String>) {
        self.text_view.get_buffer()
            .map(|buffer| {
                match value {
                    Some(text) => buffer.set_text(text),
                    None => buffer.set_text("")
                }
            });
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.text_view.get_buffer()
            .map(|buffer| {
                buffer.connect_changed(move |buffer| {
                    callback(buffer_get_text(buffer).as_ref());
                });
            });
    }
}
