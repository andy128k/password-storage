use crate::gtk_prelude::*;

fn create_style_provider() -> gtk::CssProvider {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(br##"label.error { color: #FF3333; }"##);
    provider
}

pub fn create_error_label() -> gtk::Label {
    let label = gtk::Label::new(None);
    label.set_xalign(0.0);
    label.set_yalign(0.5);
    label.hide();

    let context = label.style_context();
    context.add_class("error");
    let provider = create_style_provider();
    context.add_provider(&provider, gtk::STYLE_PROVIDER_PRIORITY_FALLBACK);

    label
}
