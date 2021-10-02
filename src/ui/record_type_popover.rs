use crate::gtk_prelude::*;
use crate::model::record::RecordType;
use crate::ui::fancy_button::fancy_button;
use crate::ui::flow_grid::flow_grid;
use std::rc::Rc;

pub fn record_type_popover(
    record_types: &[&'static RecordType],
    on_activate: impl Fn(&'static RecordType) + 'static,
) -> gtk::Popover {
    let popover = gtk::Popover::builder().build();
    let on_activate = Rc::new(on_activate);
    let buttons: Vec<gtk::Widget> = record_types
        .iter()
        .map(|record_type| {
            let record_type = *record_type;
            let button = fancy_button(record_type.icon, record_type.title);
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
    grid.show_all();
    popover.add(&grid);
    popover
}
