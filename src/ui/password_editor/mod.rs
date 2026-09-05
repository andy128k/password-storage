use crate::entropy::{AsciiClassifier, password_entropy};
use crate::password::generate_password;
use crate::ui::forms::base::FormWidget;
use crate::ui::password_strength_bar::PasswordStrengthBar;
use crate::utils::string::StringExt;
use crate::utils::style::StaticCssExt;
use gtk::{glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::utils::{grid_layout::PSGridLayoutExt, ui::orphan_all_children};
    use std::cell::{Cell, RefCell};

    #[derive(glib::Properties)]
    #[properties(wrapper_type = super::PasswordEditor)]
    pub struct PasswordEditor {
        entry: gtk::Entry,
        generate_button: gtk::Button,
        generate_button_frame: gtk::Widget,
        strength: PasswordStrengthBar,
        #[property(get, set)]
        value: RefCell<String>,
        #[property(get, set = Self::set_generate)]
        generate: Cell<bool>,
        #[property(get, set = Self::set_evaluate)]
        evaluate: Cell<bool>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PasswordEditor {
        const NAME: &'static str = "PSPasswordEditor";
        type Type = super::PasswordEditor;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            let generate_button = gtk::Button::builder()
                .icon_name("random")
                .tooltip_text("Generate password")
                .has_frame(false)
                .build();
            let generate_button_frame = square(&generate_button);

            Self {
                entry: gtk::Entry::builder()
                    .visibility(false)
                    .can_focus(true)
                    .activates_default(true)
                    .width_request(300)
                    .hexpand(true)
                    .build(),
                generate_button,
                generate_button_frame,
                strength: PasswordStrengthBar::default(),
                value: Default::default(),
                generate: Cell::new(true),
                evaluate: Cell::new(true),
            }
        }
    }

    #[glib::derived_properties]
    impl ObjectImpl for PasswordEditor {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(
                gtk::GridLayout::builder()
                    .row_spacing(5)
                    .column_spacing(5)
                    .build(),
            ));

            obj.add_static_css(
                include_str!("style.css"),
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
            obj.add_css_class("password-editor");

            self.entry.connect_changed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_| {
                    imp.strength.set_strength(
                        imp.obj()
                            .value()
                            .non_empty()
                            .map(|text| password_entropy(&AsciiClassifier, text.as_bytes()).into()),
                    );
                }
            ));

            let visibility_toggle = gtk::ToggleButton::builder()
                .icon_name("eye")
                .tooltip_text("Reveal password")
                .has_frame(false)
                .build();
            visibility_toggle.connect_clicked(glib::clone!(
                #[weak(rename_to = entry)]
                self.entry,
                move |t| entry.set_visibility(t.is_active())
            ));

            self.generate_button.connect_clicked(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_| {
                    glib::spawn_future_local(async move { imp.generate_password().await });
                }
            ));

            obj.grid_attach(&self.entry, 0, 0, 1, 1);
            obj.grid_attach(&square(&visibility_toggle), 1, 0, 1, 1);
            obj.grid_attach(&self.generate_button_frame, 2, 0, 1, 1);
            obj.grid_attach(&self.strength, 0, 1, 3, 1);

            obj.bind_property("value", &self.entry.buffer(), "text")
                .bidirectional()
                .sync_create()
                .build();
        }

        fn dispose(&self) {
            orphan_all_children(&*self.obj());
        }
    }

    impl WidgetImpl for PasswordEditor {}

    impl PasswordEditor {
        fn set_generate(&self, generate: bool) {
            self.generate.set(generate);
            self.generate_button_frame.set_visible(generate);
            self.update_layout();
        }

        fn set_evaluate(&self, evaluate: bool) {
            self.evaluate.set(evaluate);
            self.strength.set_visible(evaluate);
            self.update_layout();
        }

        fn update_layout(&self) {
            if self.evaluate.get() {
                self.obj()
                    .layout_child(&self.strength)
                    .set_column_span(2 + (self.generate.get() as i32));
            }
        }

        async fn generate_password(&self) {
            let is_empty = self.entry.text().is_empty();
            if is_empty || self.confirm_password_overwrite().await {
                let password = generate_password();
                self.entry.set_text(&password);
            }
        }

        async fn confirm_password_overwrite(&self) -> bool {
            let parent_window = self.obj().root().and_downcast::<gtk::Window>();
            let answer = gtk::AlertDialog::builder()
                .modal(true)
                .buttons(["No", "Yes"])
                .default_button(0)
                .cancel_button(0)
                .message("Do you want to overwrite current password?")
                .build()
                .choose_future(parent_window.as_ref())
                .await;
            answer == Ok(1)
        }
    }

    fn square(widget: &impl IsA<gtk::Widget>) -> gtk::Widget {
        gtk::AspectFrame::builder()
            .ratio(1.0)
            .child(widget)
            .build()
            .upcast()
    }
}

glib::wrapper! {
    pub struct PasswordEditor(ObjectSubclass<imp::PasswordEditor>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for PasswordEditor {
    fn default() -> Self {
        glib::Object::new()
    }
}

impl PasswordEditor {
    pub fn simple() -> Self {
        glib::Object::builder()
            .property("generate", false)
            .property("evaluate", false)
            .build()
    }
}

impl FormWidget<String> for PasswordEditor {
    fn get_widget(&self) -> gtk::Widget {
        self.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        self.value().non_empty()
    }

    fn set_value(&self, value: Option<&String>) {
        self.set_value(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.connect_value_notify(move |editor| {
            callback(editor.get_value().as_ref());
        });
    }
}
