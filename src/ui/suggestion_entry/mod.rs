use super::forms::base::FormWidget;
use crate::utils::string::StringExt;
use gtk::{gdk, gio, glib, prelude::*, subclass::prelude::*};

mod imp {
    use super::*;
    use crate::utils::style::StaticCssExt;
    use std::cell::RefCell;

    pub struct PSSuggestionEntry {
        pub selection: gtk::SingleSelection,
        pub entry: gtk::Entry,
        changed_id: RefCell<Option<glib::SignalHandlerId>>,
        pub popup: gtk::Popover,
        pub list: gtk::ListView,
        pub filter: gtk::StringFilter,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSSuggestionEntry {
        const NAME: &'static str = "PSSuggestionEntry";
        type Type = super::PSSuggestionEntry;
        type ParentType = gtk::Widget;

        fn new() -> Self {
            Self {
                selection: Default::default(),
                entry: Default::default(),
                changed_id: Default::default(),
                popup: Default::default(),
                list: Default::default(),
                filter: gtk::StringFilter::builder()
                    .ignore_case(true)
                    .match_mode(gtk::StringFilterMatchMode::Substring)
                    .expression(gtk::PropertyExpression::new(
                        gtk::StringObject::static_type(),
                        gtk::Expression::NONE,
                        "string",
                    ))
                    .build(),
            }
        }
    }

    impl ObjectImpl for PSSuggestionEntry {
        fn constructed(&self) {
            self.parent_constructed();

            let obj = self.obj();
            obj.set_layout_manager(Some(gtk::BinLayout::new()));

            obj.add_static_css(
                include_str!("style.css"),
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
            obj.add_css_class("suggestion");

            self.entry.set_parent(&*obj);
            let changed_id = self.entry.connect_changed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_| imp.text_changed()
            ));
            *self.changed_id.borrow_mut() = Some(changed_id);

            self.entry.connect_activate(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_| {
                    imp.set_popup_visible(false);
                    imp.accept_current_selection();
                }
            ));

            self.popup.set_position(gtk::PositionType::Bottom);
            self.popup.set_autohide(false);
            self.popup.set_has_arrow(false);
            self.popup.set_halign(gtk::Align::Start);
            self.popup.add_css_class("menu");
            self.popup.set_parent(&*obj);

            let sw = gtk::ScrolledWindow::builder()
                .hscrollbar_policy(gtk::PolicyType::Never)
                .vscrollbar_policy(gtk::PolicyType::Automatic)
                .max_content_height(400)
                .propagate_natural_height(true)
                .build();
            self.popup.set_child(Some(&sw));

            self.list.set_single_click_activate(true);
            self.list.connect_activate(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                move |_, _| {
                    imp.set_popup_visible(false);
                    imp.accept_current_selection();
                }
            ));

            let factory = gtk::SignalListItemFactory::new();
            factory.connect_setup(|_, list_item| {
                let Some(list_item) = list_item.downcast_ref::<gtk::ListItem>() else {
                    return;
                };
                let label = gtk::Label::builder().xalign(0_f32).build();
                list_item.set_child(Some(&label));
            });
            factory.connect_bind(|_, list_item| {
                let Some(list_item) = list_item.downcast_ref::<gtk::ListItem>() else {
                    return;
                };
                let Some(item) = list_item.item().and_downcast::<gtk::StringObject>() else {
                    return;
                };
                let Some(label) = list_item.child().and_downcast::<gtk::Label>() else {
                    return;
                };
                label.set_label(&item.string());
            });
            self.list.set_factory(Some(&factory));

            sw.set_child(Some(&self.list));

            let key_controller = gtk::EventControllerKey::new();
            key_controller.connect_key_pressed(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                #[upgrade_or]
                glib::Propagation::Proceed,
                move |_, keyval, _, state| imp.key_pressed(keyval, state)
            ));
            self.entry.add_controller(key_controller);

            // let leave_controller = gtk::EventControllerFocus::new();
            // leave_controller.connect_leave(glib::clone!(
            //     #[weak(rename_to = imp)]
            //     self,
            //     move |_| {
            //         if imp.popup.is_mapped() {
            //             imp.set_popup_visible(false);
            //             imp.accept_current_selection();
            //         }
            //     }
            // ));
            // self.entry.add_controller(leave_controller);
        }

        fn dispose(&self) {
            while let Some(child) = self.obj().first_child() {
                child.unparent();
            }
        }
    }

    impl WidgetImpl for PSSuggestionEntry {}

    impl PSSuggestionEntry {
        pub fn set_model(&self, model: &gio::ListModel) {
            let filter_model =
                gtk::FilterListModel::new(Some(model.clone()), Some(self.filter.clone()));

            self.selection.set_model(Some(&filter_model));
            self.selection.set_autoselect(false);
            self.selection.set_can_unselect(true);
            self.selection.set_selected(gtk::INVALID_LIST_POSITION);
            self.list.set_model(Some(&self.selection));
        }

        fn set_popup_visible(&self, visible: bool) {
            if self.popup.is_visible() == visible {
                return;
            }

            if visible {
                if !self.entry.has_focus() {
                    self.entry.grab_focus_without_selecting();
                }

                self.selection.set_selected(gtk::INVALID_LIST_POSITION);
                self.popup.popup();
            } else {
                self.popup.popdown();
            }
        }

        fn text_changed(&self) {
            let text = self.entry.text();
            self.filter.set_search(Some(text.as_str()));
            self.set_popup_visible(self.selection.n_items() > 0);
        }

        fn key_pressed(&self, keyval: gdk::Key, state: gdk::ModifierType) -> glib::Propagation {
            const PAGE_STEP: i32 = 10;

            if !state.is_empty() {
                return glib::Propagation::Proceed;
            }

            match keyval {
                gdk::Key::Return | gdk::Key::KP_Enter | gdk::Key::ISO_Enter => {
                    self.set_popup_visible(false);
                    self.accept_current_selection();
                    return glib::Propagation::Stop;
                }
                gdk::Key::Escape => {
                    if self.popup.is_mapped() {
                        self.set_popup_visible(false);
                        return glib::Propagation::Stop;
                    }
                }
                gdk::Key::Right | gdk::Key::KP_Right => {
                    self.entry.set_position(-1);
                    return glib::Propagation::Stop;
                }
                gdk::Key::Left | gdk::Key::KP_Left => {
                    return glib::Propagation::Proceed;
                }
                gdk::Key::Tab | gdk::Key::KP_Tab | gdk::Key::ISO_Left_Tab => {
                    self.set_popup_visible(false);
                    return glib::Propagation::Proceed;
                }
                _ => {}
            }

            let delta = match keyval {
                gdk::Key::Up | gdk::Key::KP_Up => Some(-1),
                gdk::Key::Down | gdk::Key::KP_Down => Some(1),
                gdk::Key::Page_Up => Some(-PAGE_STEP),
                gdk::Key::Page_Down => Some(PAGE_STEP),
                _ => None,
            };
            if let Some(delta) = delta {
                let total = self.selection.n_items();
                let selected = self.selection.selected();
                let new_selected = incr(selected, total, delta);

                self.selection.set_selected(new_selected);
                self.list
                    .scroll_to(new_selected, gtk::ListScrollFlags::SELECT, None);
                return glib::Propagation::Stop;
            }

            glib::Propagation::Proceed
        }

        fn accept_current_selection(&self) {
            let Some(item) = self.selection.selected_item() else {
                return;
            };
            let Some(value) = item.downcast_ref::<gtk::StringObject>() else {
                return;
            };

            self.set_text_without_handler(&value.string());
        }

        pub fn set_text_without_handler(&self, text: &str) {
            if let Some(ref handler_id) = *self.changed_id.borrow() {
                self.entry.block_signal(handler_id);
            }
            self.entry.set_text(text);
            self.entry.set_position(-1);
            if let Some(ref handler_id) = *self.changed_id.borrow() {
                self.entry.unblock_signal(handler_id);
            }
        }
    }
}

glib::wrapper! {
    pub struct PSSuggestionEntry(ObjectSubclass<imp::PSSuggestionEntry>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl PSSuggestionEntry {
    pub fn new(model: &gio::ListModel) -> Self {
        let this: Self = glib::Object::builder().build();
        this.imp().set_model(model);
        this
    }
}

fn incr(value: u32, size: u32, delta: i32) -> u32 {
    if value == gtk::INVALID_LIST_POSITION {
        if delta < 0 {
            size - 1
        } else if delta > 0 {
            0
        } else {
            gtk::INVALID_LIST_POSITION
        }
    } else {
        value.saturating_add_signed(delta).clamp(0, size - 1)
    }
}

impl FormWidget<String> for PSSuggestionEntry {
    fn get_widget(&self) -> gtk::Widget {
        self.clone().upcast()
    }

    fn get_value(&self) -> Option<String> {
        self.imp().entry.text().non_empty().map(|gs| gs.to_string())
    }

    fn set_value(&self, value: Option<&String>) {
        self.imp()
            .set_text_without_handler(value.map(String::as_str).unwrap_or_default());
    }

    fn connect_changed(&mut self, callback: Box<dyn Fn(Option<&String>)>) {
        self.imp().entry.connect_changed(move |entry| {
            let value = entry.text().non_empty().map(|gs| gs.to_string());
            callback(value.as_ref());
        });
    }
}
