use gtk::prelude::*;

pub fn fancy_button(icon: &str, label: &str) -> gtk::Button {
    let content = gtk::Grid::builder()
        .row_homogeneous(false)
        .row_spacing(5)
        .margin_top(10)
        .margin_bottom(10)
        .margin_start(10)
        .margin_end(10)
        .build();

    let image = gtk::Image::builder()
        .icon_name(icon)
        .icon_size(gtk::IconSize::Normal)
        .halign(gtk::Align::Center)
        .hexpand(true)
        .build();
    content.attach(&image, 0, 0, 1, 1);

    let label = gtk::Label::builder()
        .label(label)
        .halign(gtk::Align::Center)
        .hexpand(true)
        .build();
    content.attach(&label, 0, 1, 1, 1);

    gtk::Button::builder()
        .child(&content)
        .css_classes(["flat"])
        .build()
}
