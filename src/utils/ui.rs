use std::collections::VecDeque;

use crate::gtk_prelude::*;

pub struct WidgetChildrenIter(Option<gtk::Widget>);

impl Iterator for WidgetChildrenIter {
    type Item = gtk::Widget;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.0.take()?;
        self.0 = current.next_sibling();
        Some(current)
    }
}

pub struct WidgetChildrenRevIter(Option<gtk::Widget>);

impl Iterator for WidgetChildrenRevIter {
    type Item = gtk::Widget;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.0.take()?;
        self.0 = current.prev_sibling();
        Some(current)
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
        if let Some(ref container) = front {
            for child in container.children_rev() {
                self.widgets.push_front(child);
            }
        }
        front
    }
}

pub trait PSWidgetExt {
    fn children(&self) -> WidgetChildrenIter;
    fn children_rev(&self) -> WidgetChildrenRevIter;
    fn traverse(&self) -> WidgetTraverseDepthFirstIter;
}

impl<P: IsA<gtk::Widget>> PSWidgetExt for P {
    fn children(&self) -> WidgetChildrenIter {
        WidgetChildrenIter(self.first_child())
    }

    fn children_rev(&self) -> WidgetChildrenRevIter {
        WidgetChildrenRevIter(self.last_child())
    }

    fn traverse(&self) -> WidgetTraverseDepthFirstIter {
        WidgetTraverseDepthFirstIter::new(self.clone().upcast())
    }
}
pub trait PSWidgetLookupExt {
    fn of_type<W: IsA<gtk::Widget>>(&self) -> Option<W>;
    fn of_type_and_name<W: IsA<gtk::Widget>>(&self, name: &str) -> Option<W>;
}

impl PSWidgetLookupExt for gtk::Widget {
    fn of_type<W: IsA<gtk::Widget>>(&self) -> Option<W> {
        self.traverse().find_map(|ch| ch.downcast::<W>().ok())
    }

    fn of_type_and_name<W: IsA<gtk::Widget>>(&self, name: &str) -> Option<W> {
        self.traverse()
            .filter(|ch| ch.widget_name() == name)
            .find_map(|ch| ch.downcast::<W>().ok())
    }
}

impl PSWidgetLookupExt for Option<gtk::Widget> {
    fn of_type<W: IsA<gtk::Widget>>(&self) -> Option<W> {
        self.as_ref().and_then(|w| w.of_type::<W>())
    }

    fn of_type_and_name<W: IsA<gtk::Widget>>(&self, name: &str) -> Option<W> {
        self.as_ref().and_then(|w| w.of_type_and_name::<W>(name))
    }
}

pub fn centered<W: IsA<gtk::Widget>>(widget: &W) -> gtk::Widget {
    widget.set_hexpand(true);
    widget.set_halign(gtk::Align::Center);

    let grid = gtk::Grid::new();
    grid.attach(widget, 0, 0, 1, 1);
    grid.upcast()
}

pub fn scrolled<P: IsA<gtk::Widget>>(widget: &P) -> gtk::ScrolledWindow {
    gtk::ScrolledWindow::builder()
        .can_focus(true)
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .has_frame(true)
        .hexpand(true)
        .vexpand(true)
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

pub fn overlayed<P1: IsA<gtk::Widget>, P2: IsA<gtk::Widget>>(
    widget: &P1,
    overlay: &P2,
) -> gtk::Overlay {
    let overlay_container = gtk::Overlay::builder().child(widget).build();
    overlay_container.add_overlay(overlay);
    overlay_container
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
            .unwrap_or_else(|| panic!("Action {} should exist.", name))
            .downcast::<gio::SimpleAction>()
            .unwrap_or_else(|_| panic!("Action {} should be a simple action.", name))
    }
}

pub fn action_button(action: &str, icon: &str, title: &str) -> gtk::Button {
    gtk::Button::builder()
        .action_name(action)
        .icon_name(icon)
        .has_tooltip(true)
        .tooltip_text(title)
        .has_frame(false)
        .build()
}

pub fn action_popover_button(popover: &gtk::Popover, icon: &str, title: &str) -> gtk::MenuButton {
    gtk::MenuButton::builder()
        .popover(popover)
        .icon_name(icon)
        .has_tooltip(true)
        .tooltip_text(title)
        .has_frame(false)
        .build()
}

pub fn linked_button_box() -> gtk::Box {
    let bbox = gtk::Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .build();
    bbox.style_context().add_class("linked");
    bbox
}

fn title_label(label: &str, class: &str) -> gtk::Label {
    let label = gtk::Label::builder()
        .label(label)
        .single_line_mode(true)
        .ellipsize(pango::EllipsizeMode::End)
        .build();
    label.add_css_class(class);
    label
}

pub fn title(title: &str) -> gtk::Widget {
    title_label(title, "title").upcast()
}

pub fn title_and_subtitle(title: &str, subtitle: &str) -> gtk::Widget {
    let vbox = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .valign(gtk::Align::Center)
        .build();
    vbox.append(&title_label(title, "title"));
    vbox.append(&title_label(subtitle, "subtitle"));
    vbox.upcast()
}
