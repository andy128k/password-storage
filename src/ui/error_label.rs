pub fn create_error_label() -> gtk::Label {
    gtk::Label::builder()
        .xalign(0.0)
        .yalign(0.5)
        .visible(false)
        .css_classes(["error"])
        .build()
}
