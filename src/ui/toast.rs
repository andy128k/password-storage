use gtk::{glib, prelude::*};
use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};

#[derive(glib::Downgrade)]
pub struct Toast {
    revealer: gtk::Revealer,
    label: gtk::Label,
    close_at: Rc<Cell<Instant>>,
}

impl Default for Toast {
    fn default() -> Self {
        let revealer = gtk::Revealer::builder()
            .halign(gtk::Align::Center)
            .valign(gtk::Align::Start)
            .build();

        let close_button = gtk::Button::builder()
            .icon_name("window-close-symbolic")
            .build();
        close_button.style_context().add_class("close-button");

        let label = gtk::Label::builder().build();

        let grid = gtk::Grid::builder().column_spacing(10).build();
        grid.attach(&close_button, 0, 0, 1, 1);
        grid.attach(&label, 1, 0, 1, 1);

        let event_controller = gtk::EventControllerMotion::builder().build();

        let frame = gtk::Frame::builder().child(&grid).build();
        frame.style_context().add_class("app-notification");
        frame.add_controller(event_controller.clone());

        revealer.set_child(Some(&frame));

        let toast = Self {
            revealer,
            label,
            close_at: Rc::new(Cell::new(past())),
        };

        event_controller.connect_enter(glib::clone!(@weak toast => move |_, _, _| {
            toast.close_at.set(far_future());
        }));

        event_controller.connect_leave(glib::clone!(@weak toast => move |_| {
            toast.close_at.set(future());
        }));

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
}

impl Toast {
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
