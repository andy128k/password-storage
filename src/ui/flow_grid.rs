use gtk::prelude::*;

pub fn flow_grid(columns: usize, widgets: &[gtk::Widget]) -> gtk::Grid {
    let grid = gtk::Grid::builder()
        .margin_top(10)
        .margin_bottom(10)
        .margin_start(10)
        .margin_end(10)
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
