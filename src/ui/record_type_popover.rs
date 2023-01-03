use crate::gtk_prelude::*;
use crate::model::record::RecordType;
use crate::ui::fancy_button::fancy_button;
use crate::ui::flow_grid::flow_grid;
use std::rc::Rc;

pub struct RecordTypePopoverBuilder<'b> {
    record_types: &'b [&'static RecordType],
    on_activate: Rc<dyn Fn(&'static RecordType)>,
    action_name_func: Option<Box<dyn Fn(&'static RecordType) -> String + 'b>>,
}

impl std::default::Default for RecordTypePopoverBuilder<'static> {
    fn default() -> Self {
        Self {
            record_types: &[],
            on_activate: Rc::new(|_| {}),
            action_name_func: None,
        }
    }
}

impl<'b> RecordTypePopoverBuilder<'b> {
    pub fn record_types(mut self, record_types: &'b [&'static RecordType]) -> Self {
        self.record_types = record_types;
        self
    }

    pub fn action_name_func(
        mut self,
        action_name_func: impl Fn(&'static RecordType) -> String + 'b,
    ) -> Self {
        self.action_name_func = Some(Box::new(action_name_func));
        self
    }

    pub fn on_activate(mut self, on_activate: impl Fn(&'static RecordType) + 'static) -> Self {
        self.on_activate = Rc::new(on_activate);
        self
    }

    pub fn build(self) -> gtk::Popover {
        let popover = gtk::Popover::builder().build();
        let on_activate = self.on_activate;
        let action_name_func = self.action_name_func;
        let buttons: Vec<gtk::Widget> = self
            .record_types
            .iter()
            .map(|record_type| {
                let record_type = *record_type;
                let button = fancy_button(record_type.icon, record_type.title);
                if let Some(ref action_name_func) = action_name_func {
                    button.set_detailed_action_name(&(action_name_func)(record_type));
                }
                button.connect_clicked(
                    glib::clone!(@weak popover, @strong on_activate => move |_| {
                        popover.popdown();
                        on_activate(record_type);
                    }),
                );
                button.upcast()
            })
            .collect();
        let grid = flow_grid(3, &buttons);
        popover.set_child(Some(&grid));
        popover
    }
}
