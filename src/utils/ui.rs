use crate::gtk_prelude::*;

pub fn scrolled<P: IsA<gtk::Widget>>(widget: &P) -> gtk::ScrolledWindow {
    gtk::ScrolledWindow::builder()
        .can_focus(true)
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .has_frame(true)
        .child(widget)
        .build()
}

pub fn paned<P1: IsA<gtk::Widget>, P2: IsA<gtk::Widget>>(pane1: &P1, pane2: &P2) -> gtk::Paned {
    gtk::Paned::builder()
        .orientation(gtk::Orientation::Horizontal)
        .hexpand(true)
        .vexpand(true)
        .start_child(pane1)
        .resize_start_child(true)
        .shrink_start_child(false)
        .end_child(pane2)
        .resize_end_child(false)
        .shrink_end_child(false)
        .build()
}

pub trait PSStackExt {
    fn named<P: IsA<gtk::Widget>>(self, name: &str, child: &P) -> Self;
}

impl PSStackExt for gtk::Stack {
    fn named<P: IsA<gtk::Widget>>(self, name: &str, child: &P) -> Self {
        self.add_named(child, Some(name));
        self
    }
}

pub trait PSSimpleActionGroupExt {
    fn simple_actions(&self) -> Vec<gio::SimpleAction>;
    fn simple_action(&self, name: &str) -> gio::SimpleAction;
    fn set_enabled(&self, enabled: bool) {
        for action in self.simple_actions() {
            action.set_enabled(enabled);
        }
    }
}

impl PSSimpleActionGroupExt for gio::SimpleActionGroup {
    fn simple_actions(&self) -> Vec<gio::SimpleAction> {
        self.list_actions()
            .iter()
            .filter_map(|name| self.lookup_action(name))
            .filter_map(|action| action.downcast::<gio::SimpleAction>().ok())
            .collect()
    }

    fn simple_action(&self, name: &str) -> gio::SimpleAction {
        self.lookup_action(name)
            .expect(&format!("Action {} should exist.", name))
            .downcast::<gio::SimpleAction>()
            .expect(&format!("Action {} should be a simple action.", name))
    }
}
