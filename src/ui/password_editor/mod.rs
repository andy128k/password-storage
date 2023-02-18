use crate::entropy::{password_entropy, AsciiClassifier};
use crate::password::generate_password;
use crate::ui::dialogs::ask::confirm_unlikely;
use crate::ui::forms::base::FormWidget;
use crate::ui::password_strength_bar::PasswordStrenthBar;
use crate::utils::string::StringExt;
use crate::utils::style::StaticCssExt;
use gtk::{glib, prelude::*};

pub struct PasswordEditor {
    container: gtk::Grid,
    entry: gtk::Entry,
}

async fn confirm_password_overwrite<P: WidgetExt>(widget: &P) -> bool {
    if let Some(window) = widget.root().and_downcast() {
        confirm_unlikely(&window, "Do you want to overwrite current password?").await
    } else {
        false
    }
}

async fn generate_password_clicked(entry: gtk::Entry) {
    let is_empty = entry.text().is_empty();
    if is_empty || confirm_password_overwrite(&entry).await {
        let password = generate_password();
        entry.set_text(&password);
    }
}

impl PasswordEditor {
    pub fn new() -> Self {
        let level = PasswordStrenthBar::new();

        let entry = gtk::Entry::builder()
            .visibility(false)
            .can_focus(true)
            .activates_default(true)
            .width_request(300)
            .hexpand(true)
            .build();
        entry.connect_changed(glib::clone!(@weak level => move |e| {
            let strength = get_value(e)
                .map(|text| password_entropy(&AsciiClassifier, text.as_bytes()).into());
            level.set_strength(strength);
        }));

        let visibility_toggle = gtk::ToggleButton::builder()
            .icon_name("eye")
            .tooltip_text("Reveal password")
            .has_frame(false)
            .build();
        visibility_toggle.connect_clicked(glib::clone!(@weak entry => move |t| {
            entry.set_visibility(t.is_active());
        }));

        let generate_button = gtk::Button::builder()
            .icon_name("random")
            .tooltip_text("Generate password")
            .has_frame(false)
            .build();
        generate_button.connect_clicked(glib::clone!(@weak entry => move |_| {
            glib::MainContext::default().spawn_local(
                generate_password_clicked(entry)
            );
        }));

        let container = gtk::Grid::builder()
            .row_spacing(5)
            .column_spacing(5)
            .css_classes(["password-editor"])
            .build();
        container.add_static_css(
            include_str!("style.css"),
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
        container.attach(&entry, 0, 0, 1, 1);
        container.attach(&square(visibility_toggle), 1, 0, 1, 1);
        container.attach(&square(generate_button), 2, 0, 1, 1);
        container.attach(&level.get_widget(), 0, 1, 3, 1);

        Self { container, entry }
    }
}

impl FormWidget<String> for PasswordEditor {
    fn get_widget(&self) -> gtk::Widget {
        self.container.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        get_value(&self.entry)
    }

    fn set_value(&self, value: Option<&String>) {
        self.entry
            .set_text(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.entry.connect_changed(move |entry| {
            let value = get_value(entry);
            callback(value.as_ref());
        });
    }
}

fn get_value(entry: &gtk::Entry) -> Option<String> {
    entry.text().non_empty().map(|gs| gs.to_string())
}

fn square(widget: impl IsA<gtk::Widget>) -> gtk::Widget {
    gtk::AspectFrame::builder()
        .ratio(1.0)
        .child(&widget)
        .build()
        .upcast()
}
