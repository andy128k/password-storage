use super::base::*;
use crate::utils::string::StringExt;
use gtk::prelude::*;

pub struct MultiLine {
    scrolled_window: gtk::ScrolledWindow,
    text_view: gtk::TextView,
}

impl Default for MultiLine {
    fn default() -> Self {
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
            .has_frame(true)
            .hexpand(true)
            .vexpand(true)
            .width_request(300)
            .height_request(200)
            .child(&text_view)
            .build();

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
        .non_empty()
        .map(|gs| gs.to_string())
}

impl FormWidget<String> for MultiLine {
    fn get_widget(&self) -> gtk::Widget {
        self.scrolled_window.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        buffer_get_text(&self.text_view.buffer())
    }

    fn set_value(&self, value: Option<&String>) {
        self.text_view
            .buffer()
            .set_text(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.text_view.buffer().connect_changed(move |buffer| {
            callback(buffer_get_text(buffer).as_ref());
        });
    }
}
