use gtk::prelude::*;

fn create_style_provider() -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider
        .load_from_data(br##"label.error { color: #FF3333; }"##)
        .expect("CSS is loaded");
    provider
}

pub fn create_error_label() -> gtk::Label {
    let label = gtk::Label::new(None);
    label.set_xalign(0.5);
    label.set_yalign(0.5);
    label.set_no_show_all(true);

    let context = label.style_context();
    context.add_class(&gtk::STYLE_CLASS_ERROR);
    let provider = create_style_provider();
    context.add_provider(&provider, gtk::STYLE_PROVIDER_PRIORITY_FALLBACK);

    label
}
