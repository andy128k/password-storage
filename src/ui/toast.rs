use crate::gtk_prelude::*;
use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};

#[derive(glib::Downgrade)]
pub struct Toast {
    revealer: gtk::Revealer,
    label: gtk::Label,
    close_at: Rc<Cell<Instant>>,
}

impl Toast {
    pub fn new() -> Self {
        let revealer = gtk::Revealer::builder()
            .halign(gtk::Align::Center)
            .valign(gtk::Align::Start)
            .build();

        let image = gtk::Image::from_icon_name(Some("window-close-symbolic"), gtk::IconSize::Menu);
        let close_button = gtk::Button::builder().image(&image).build();
        close_button.style_context().add_class("close-button");

        let label = gtk::Label::builder().build();

        let grid = gtk::Grid::builder().column_spacing(10).build();
        grid.attach(&close_button, 0, 0, 1, 1);
        grid.attach(&label, 1, 0, 1, 1);

        let frame = gtk::Frame::builder().build();
        frame.style_context().add_class("app-notification");
        frame.add(&grid);

        let event_box = gtk::EventBox::builder()
            .events(gdk::EventMask::ENTER_NOTIFY_MASK | gdk::EventMask::LEAVE_NOTIFY_MASK)
            .build();
        event_box.add(&frame);

        revealer.add(&event_box);

        let toast = Self {
            revealer,
            label,
            close_at: Rc::new(Cell::new(past())),
        };

        event_box.connect_enter_notify_event(
            glib::clone!(@weak toast => @default-return glib::signal::Inhibit(false), move |_, _| {
                toast.close_at.set(far_future());
                glib::signal::Inhibit(false)
            }),
        );

        event_box.connect_leave_notify_event(
            glib::clone!(@weak toast => @default-return glib::signal::Inhibit(false), move |_, _| {
                toast.close_at.set(future());
                glib::signal::Inhibit(false)
            }),
        );

        close_button.connect_clicked(glib::clone!(@weak toast => move |_| {
            toast.close_at.set(past());
            toast.revealer.set_reveal_child(false);
        }));

        glib::timeout_add_local(
            Duration::from_millis(100),
            glib::clone!(@weak toast => @default-return glib::Continue(false), move || {
                toast.tick();
                glib::Continue(true)
            }),
        );

        toast
    }

    pub fn as_widget(&self) -> gtk::Widget {
        self.revealer.clone().upcast()
    }

    pub fn notify(&self, title: &str) {
        self.close_at.set(future());
        self.label.set_label(title);
        self.revealer.set_reveal_child(true);
    }

    fn tick(&self) {
        if self.close_at.get() < Instant::now() {
            self.revealer.set_reveal_child(false);
        }
    }
}

fn past() -> Instant {
    Instant::now() - Duration::from_millis(1)
}

fn future() -> Instant {
    Instant::now() + Duration::from_millis(2000)
}

fn far_future() -> Instant {
    Instant::now() + Duration::from_secs(3600)
}
