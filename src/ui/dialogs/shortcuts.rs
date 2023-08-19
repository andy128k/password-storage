use crate::compat::accel::PRIMARY_MODIFIER;
use crate::shortcuts::ShortcutGroup;
use crate::ui::accel_label::AccelLabel;
use gtk::{gdk, glib, prelude::*};

pub fn shortcuts_window(
    parent: Option<&impl glib::IsA<gtk::Window>>,
    shortcuts: &[ShortcutGroup<'static>],
) -> gtk::Window {
    let window = gtk::Window::builder()
        .modal(true)
        .titlebar(&gtk::HeaderBar::new())
        .title("Shortcuts")
        .resizable(false)
        .build();
    window.set_transient_for(parent);

    let key_controller = gtk::EventControllerKey::new();
    key_controller.connect_key_pressed(
        glib::clone!(@weak window => @default-return glib::Propagation::Proceed, move |_controller, key, _keycode, modifier| {
            const NO_MODIFIER: gdk::ModifierType = gdk::ModifierType::empty();
            match (key, modifier) {
                (gdk::Key::Escape, NO_MODIFIER) |
                (gdk::Key::w, PRIMARY_MODIFIER) |
                (gdk::Key::W, PRIMARY_MODIFIER) => {
                    window.close();
                    glib::Propagation::Stop
                },
                _ => glib::Propagation::Proceed,
            }
        }),
    );
    window.add_controller(key_controller);

    let grid = gtk::Grid::builder()
        .orientation(gtk::Orientation::Vertical)
        .row_spacing(10)
        .column_spacing(10)
        .margin_top(20)
        .margin_bottom(40)
        .margin_start(40)
        .margin_end(40)
        .build();
    window.set_child(Some(&grid));

    let mut row = 0;
    for (group_title, actions) in shortcuts {
        let group_label = gtk::Label::builder()
            .label(*group_title)
            .margin_top(10)
            .xalign(0_f32)
            .yalign(1_f32)
            .build();
        group_label.add_css_class("bold");

        grid.attach(&group_label, 0, row, 2, 1);
        row += 1;

        for shortcut in actions.iter() {
            let accel_label = AccelLabel::new(shortcut.display_accel);

            let title_label = gtk::Label::builder()
                .label(shortcut.title)
                .xalign(0_f32)
                .build();

            grid.attach(&accel_label, 0, row, 1, 1);
            grid.attach(&title_label, 1, row, 1, 1);
            row += 1;
        }
    }

    window.upcast()
}
