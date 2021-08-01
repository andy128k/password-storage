use crate::gtk_prelude::*;

// pub trait PSWidgetExt {
//     fn halign(self, align: gtk::Align) -> Self;
//     fn valign(self, align: gtk::Align) -> Self;
// }

// impl PSWidgetExt for gtk::Widget {
//     fn halign(self, align: gtk::Align) -> Self {
//         self.set_halign(align);
//         self
//     }

//     fn valign(self, align: gtk::Align) -> Self {
//         self.set_valign(align);
//         self
//     }
// }

pub fn scrolled<P: IsA<gtk::Widget>>(widget: &P) -> gtk::ScrolledWindow {
    let sw = gtk::ScrolledWindow::builder()
        .can_focus(true)
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .shadow_type(gtk::ShadowType::In)
        .build();
    sw.add(widget);
    sw
}

pub fn paned<P1: IsA<gtk::Widget>, P2: IsA<gtk::Widget>>(pane1: &P1, pane2: &P2) -> gtk::Paned {
    let paned = gtk::Paned::builder()
        .orientation(gtk::Orientation::Horizontal)
        .hexpand(true)
        .vexpand(true)
        .build();
    paned.pack1(pane1, true, false);
    paned.pack2(pane2, false, false);
    paned
}

pub trait PSStackExt {
    fn named<P: IsA<gtk::Widget>>(self, name: &str, child: &P) -> Self;
}

impl PSStackExt for gtk::Stack {
    fn named<P: IsA<gtk::Widget>>(self, name: &str, child: &P) -> Self {
        self.add_named(child, name);
        self
    }
}
