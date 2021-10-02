use crate::gtk_prelude::*;

pub fn flow_grid(columns: usize, widgets: &[gtk::Widget]) -> gtk::Grid {
    let grid = gtk::Grid::builder()
        .margin(10)
        .row_spacing(5)
        .column_spacing(5)
        .row_homogeneous(true)
        .column_homogeneous(true)
        .build();
    for (index, widget) in widgets.iter().enumerate() {
        grid.attach(
            widget,
            (index % columns) as i32,
            (index / columns) as i32,
            1,
            1,
        );
    }
    grid
}
