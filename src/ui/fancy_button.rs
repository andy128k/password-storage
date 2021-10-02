use crate::gtk_prelude::*;

pub fn fancy_button(icon: &str, label: &str) -> gtk::Button {
    let content = gtk::Grid::builder()
        .row_homogeneous(false)
        .row_spacing(5)
        .margin(10)
        .build();

    let image = gtk::Image::from_icon_name(Some(icon), gtk::IconSize::LargeToolbar);
    image.set_halign(gtk::Align::Center);
    image.set_hexpand(true);
    content.attach(&image, 0, 0, 1, 1);

    let label = gtk::Label::builder()
        .label(label)
        .halign(gtk::Align::Center)
        .hexpand(true)
        .build();
    content.attach(&label, 0, 1, 1, 1);

    let button = gtk::Button::builder().child(&content).build();
    button.style_context().add_class("flat");
    button
}
