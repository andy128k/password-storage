use crate::compat::accel::PRIMARY_MODIFIER;
use crate::shortcuts::ShortcutGroup;
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

    let section = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(10)
        .margin_top(20)
        .margin_bottom(20)
        .margin_start(20)
        .margin_end(20)
        .build();
    for (group_title, actions) in shortcuts {
        let group = gtk::ShortcutsGroup::builder().title(*group_title).build();
        for shortcut in actions.iter() {
            let s = gtk::ShortcutsShortcut::builder()
                .shortcut_type(gtk::ShortcutType::Accelerator)
                .accelerator(shortcut.accel)
                .title(shortcut.title)
                .build();
            s.set_action_name(shortcut.action);
            group.append(&s);
        }
        section.append(&group);
    }
    window.set_child(Some(&section));

    window.upcast()
}
