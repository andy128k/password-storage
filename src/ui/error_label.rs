use gtk::prelude::*;
use gtk::{Label, STYLE_CLASS_ERROR, STYLE_PROVIDER_PRIORITY_FALLBACK, CssProvider};
use crate::error::*;

fn create_style_provider() -> Result<CssProvider> {
    let provider = CssProvider::new();
    provider.load_from_data(r##"label.error { color: #FF3333; }"##.as_bytes())?;
    Ok(provider)
}

pub fn create_error_label() -> Result<Label> {
    let label = Label::new(None);
    label.set_xalign(0.5);
    label.set_yalign(0.5);
    label.set_no_show_all(true);

    let context = label.get_style_context();
    context.add_class(&STYLE_CLASS_ERROR);
    let provider = create_style_provider()?;
    context.add_provider(&provider, STYLE_PROVIDER_PRIORITY_FALLBACK);

    Ok(label)
}
