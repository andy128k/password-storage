use gtk::prelude::*;

pub trait PSGridLayoutExt {
    fn layout_child(&self, child: &impl IsA<gtk::Widget>) -> gtk::GridLayoutChild;

    fn grid_attach(
        &self,
        child: &impl IsA<gtk::Widget>,
        column: i32,
        row: i32,
        column_span: i32,
        row_span: i32,
    );
}

impl<T: IsA<gtk::Widget>> PSGridLayoutExt for T {
    fn layout_child(&self, child: &impl IsA<gtk::Widget>) -> gtk::GridLayoutChild {
        self.layout_manager()
            .and_downcast::<gtk::GridLayout>()
            .expect("GridLayout is expected")
            .layout_child(child)
            .downcast::<gtk::GridLayoutChild>()
            .expect("GridLayoutChild is expected")
    }

    fn grid_attach(
        &self,
        child: &impl IsA<gtk::Widget>,
        column: i32,
        row: i32,
        column_span: i32,
        row_span: i32,
    ) {
        child.set_parent(self);
        let lc = self.layout_child(child);
        lc.set_column(column);
        lc.set_row(row);
        lc.set_column_span(column_span);
        lc.set_row_span(row_span);
    }
}
