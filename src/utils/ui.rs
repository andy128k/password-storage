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
    paned.set_focus_chain(&[pane1.clone().upcast(), pane2.clone().upcast()]);
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

pub fn action_button(action: &str, icon: &str, title: &str) -> gtk::Button {
    gtk::Button::builder()
        .action_name(action)
        .image(&tool_icon(icon))
        .has_tooltip(true)
        .tooltip_text(title)
        .relief(gtk::ReliefStyle::None)
        .build()
}

pub fn action_menu_button(menu: &gio::Menu, icon: &str, title: &str) -> gtk::MenuButton {
    gtk::MenuButton::builder()
        .menu_model(menu)
        .image(&tool_icon(icon))
        .has_tooltip(true)
        .tooltip_text(title)
        .use_popover(false) // TODO: popover doesn't show icons
        .relief(gtk::ReliefStyle::None)
        .build()
}

fn tool_icon(icon: &str) -> gtk::Image {
    gtk::Image::from_icon_name(Some(icon), gtk::IconSize::SmallToolbar)
}
