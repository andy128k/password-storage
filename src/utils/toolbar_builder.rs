use gtk::{gio, prelude::*};

pub struct ToolbarBuilder {
    toolbar: gtk::Box,
}

impl ToolbarBuilder {
    pub fn new() -> Self {
        Self {
            toolbar: gtk::Box::builder()
                .orientation(gtk::Orientation::Horizontal)
                .spacing(4)
                .margin_start(4)
                .margin_end(4)
                .margin_top(4)
                .margin_bottom(4)
                .build(),
        }
    }

    pub fn add<P: IsA<gtk::Widget>>(self, child: &P) -> Self {
        self.toolbar.pack_start(child, false, false, 0);
        self
    }

    pub fn button(self, label: &str, icon: &str, action: &str) -> Self {
        self.add(&{
            gtk::Button::builder()
                .image(&tool_icon(icon))
                .action_name(action)
                .has_tooltip(true)
                .tooltip_text(label)
                .relief(gtk::ReliefStyle::None)
                .build()
        })
    }

    pub fn toggle_button(self, label: &str, icon: &str, action: &str) -> Self {
        self.add(&{
            gtk::ToggleButton::builder()
                .image(&tool_icon(icon))
                .action_name(action)
                .has_tooltip(true)
                .tooltip_text(label)
                .relief(gtk::ReliefStyle::None)
                .build()
        })
    }

    pub fn menu_button(self, label: &str, icon: &str, menu: &gio::Menu) -> Self {
        self.add(&{
            gtk::MenuButton::builder()
                .image(&tool_icon(icon))
                .has_tooltip(true)
                .tooltip_text(label)
                .menu_model(menu)
                .use_popover(false)
                .relief(gtk::ReliefStyle::None)
                .build()
        })
    }

    pub fn separator(self) -> Self {
        self.add(&{
            gtk::Separator::builder()
                .orientation(gtk::Orientation::Vertical)
                .build()
        })
    }

    pub fn expander(self) -> Self {
        self.add(&{
            gtk::Separator::builder()
                .orientation(gtk::Orientation::Vertical)
                .hexpand(true)
                .opacity(0.0)
                .build()
        })
    }

    pub fn build(self) -> gtk::Widget {
        self.toolbar.upcast()
    }
}

fn tool_icon(icon: &str) -> gtk::Image {
    gtk::Image::from_icon_name(Some(icon), gtk::IconSize::SmallToolbar)
}
