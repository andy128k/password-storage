use std::collections::VecDeque;

use super::also::Also;
use crate::gtk_prelude::*;

pub fn remove_all_children<W: IsA<gtk::Container>>(widget: &W) {
    for child in widget.children() {
        widget.remove(&child);
    }
}

pub struct WidgetTraverseDepthFirstIter {
    widgets: VecDeque<gtk::Widget>,
}

impl WidgetTraverseDepthFirstIter {
    pub fn new(widget: gtk::Widget) -> Self {
        let mut widgets = VecDeque::new();
        widgets.push_front(widget);
        Self { widgets }
    }
}

impl Iterator for WidgetTraverseDepthFirstIter {
    type Item = gtk::Widget;

    fn next(&mut self) -> Option<Self::Item> {
        let front = self.widgets.pop_front();
        if let Some(container) = front
            .as_ref()
            .and_then(|widget| widget.clone().downcast::<gtk::Container>().ok())
        {
            for child in container.children().into_iter().rev() {
                self.widgets.push_front(child);
            }
        }
        front
    }
}

pub fn traverse_depth_first(widget: &gtk::Widget) -> WidgetTraverseDepthFirstIter {
    WidgetTraverseDepthFirstIter::new(widget.clone())
}

pub fn scrolled<P: IsA<gtk::Widget>>(widget: &P) -> gtk::ScrolledWindow {
    gtk::ScrolledWindow::builder()
        .can_focus(true)
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .shadow_type(gtk::ShadowType::In)
        .hexpand(true)
        .vexpand(true)
        .build()
        .also(|sw| sw.add(widget))
}

pub fn paned<P1: IsA<gtk::Widget>, P2: IsA<gtk::Widget>>(pane1: &P1, pane2: &P2) -> gtk::Paned {
    gtk::Paned::builder()
        .orientation(gtk::Orientation::Horizontal)
        .hexpand(true)
        .vexpand(true)
        .build()
        .also(|paned| paned.pack1(pane1, true, false))
        .also(|paned| paned.pack2(pane2, false, false))
        .also(|paned| paned.set_focus_chain(&[pane1.clone().upcast(), pane2.clone().upcast()]))
}

pub fn overlayed<P1: IsA<gtk::Widget>, P2: IsA<gtk::Widget>>(
    widget: &P1,
    overlay: &P2,
) -> gtk::Overlay {
    gtk::Overlay::builder().build().also(|o| {
        o.add(widget);
        o.add_overlay(overlay);
    })
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
            .unwrap_or_else(|| panic!("Action {} should exist.", name))
            .downcast::<gio::SimpleAction>()
            .unwrap_or_else(|_| panic!("Action {} should be a simple action.", name))
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

pub fn action_popover_button(popover: &gtk::Popover, icon: &str, title: &str) -> gtk::MenuButton {
    gtk::MenuButton::builder()
        .popover(popover)
        .image(&tool_icon(icon))
        .has_tooltip(true)
        .tooltip_text(title)
        .relief(gtk::ReliefStyle::None)
        .build()
}

pub fn tool_icon(icon: &str) -> gtk::Image {
    gtk::Image::from_icon_name(Some(icon), gtk::IconSize::SmallToolbar)
}

pub fn linked_button_box() -> gtk::Box {
    let bbox = gtk::Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .build();
    bbox.style_context().add_class("linked");
    bbox
}
