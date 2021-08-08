use crate::gtk_prelude::*;

pub fn scrolled<P: IsA<gtk::Widget>>(widget: &P) -> gtk::ScrolledWindow {
    let sw = gtk::ScrolledWindow::builder()
        .can_focus(true)
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .shadow_type(gtk::ShadowType::In)
        .build();
    sw.add(widget);
    sw
}

pub fn paned<P1: IsA<gtk::Widget>, P2: IsA<gtk::Widget>>(pane1: &P1, pane2: &P2) -> gtk::Paned {
    let paned = gtk::Paned::builder()
        .orientation(gtk::Orientation::Horizontal)
        .hexpand(true)
        .vexpand(true)
        .build();
    paned.pack1(pane1, true, false);
    paned.pack2(pane2, false, false);
    paned
}

pub trait PSStackExt {
    fn named<P: IsA<gtk::Widget>>(self, name: &str, child: &P) -> Self;
}

impl PSStackExt for gtk::Stack {
    fn named<P: IsA<gtk::Widget>>(self, name: &str, child: &P) -> Self {
        self.add_named(child, name);
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
