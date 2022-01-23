use crate::gtk_prelude::*;
use crate::main_window::PSMainWindow;
use crate::ui::dialogs::about::about;
use crate::ui::dialogs::preferences::preferences;
use os_str_bytes::OsStringBytes;
use std::error::Error;
use std::path::PathBuf;

mod implementation;
mod shortcuts;

glib::wrapper! {
    pub struct PSApplication(ObjectSubclass<implementation::PSApplication>)
        @extends gtk::Application, gio::Application,
        @implements gio::ActionMap;
}

impl Default for PSApplication {
    fn default() -> Self {
        glib::Object::new(&[
            ("application-id", &"net.andy128k.password-storage"),
            ("flags", &gio::ApplicationFlags::HANDLES_OPEN),
        ])
        .expect("Application is created")
    }
}

impl PSApplication {
    fn on_startup(&self) {
        let private = implementation::PSApplication::from_instance(self);

        private.cache.load();

        if let Err(error) = configure() {
            eprintln!("Failed to configure global settings: {}.", error);
        }

        for (_group_title, actions) in shortcuts::SHORTCUTS {
            for shortcuts::Shortcut { action, accel, .. } in actions.iter() {
                if let Some(action) = action {
                    self.set_accels_for_action(action, &[accel]);
                }
            }
        }

        crate::icons::load_icons().unwrap();
    }

    fn on_activate(&self) {
        self.activate_main_window();
    }

    fn on_shutdown(&self) {
        let private = implementation::PSApplication::from_instance(self);
        private.cache.save().unwrap();
    }

    fn active_main_window(&self) -> Option<PSMainWindow> {
        self.active_window()
            .and_then(|w| PSMainWindow::from_window(&w))
    }

    fn activate_main_window(&self) -> PSMainWindow {
        if let Some(win) = self.active_main_window() {
            win
        } else {
            self.new_window()
        }
    }

    fn new_window(&self) -> PSMainWindow {
        let private = implementation::PSApplication::from_instance(self);
        PSMainWindow::new(&self.clone().upcast(), &private.config, &private.cache)
    }

    async fn on_open(&self, files: &[gio::File], _hint: &str) {
        if let Some(path) = files[0].path() {
            let win = self.activate_main_window();
            win.do_open_file(&path).await;
        }
    }
}

#[awesome_glib::actions]
impl PSApplication {
    fn quit(&self) {
        for window in self.windows() {
            if let Some(win) = PSMainWindow::from_window(&window) {
                win.close();
            }
        }
    }

    async fn about(&self) {
        let win = self.active_window();
        about(win.as_ref()).await;
    }

    async fn preferences(&self) {
        let win = self.activate_main_window();
        let private = implementation::PSApplication::from_instance(self);
        let config = private.config.get();
        if let Some(new_config) = preferences(&win.clone().upcast(), &config).await {
            private.config.set(new_config);
        }
    }

    #[action(name = "new")]
    async fn new_file(&self) {
        if let Some(win) = self.active_main_window() {
            win.new_file().await;
        } else {
            self.new_window();
        }
    }

    #[action(name = "open")]
    async fn open_file(&self) {
        let win = self.activate_main_window();
        win.open_file().await;
    }

    #[action(name = "open-file")]
    async fn open_file_by_name(&self, buffer: Vec<u8>) {
        match PathBuf::from_raw_vec(buffer) {
            Ok(filename) => {
                let win = self.activate_main_window();
                win.do_open_file(&filename).await;
            }
            Err(error) => eprintln!("open-file: {}", error),
        }
    }

    #[action(name = "shortcuts")]
    fn action_shortcuts(&self) {
        let window = gtk::ShortcutsWindow::builder().modal(true).build();
        window.set_transient_for(self.active_window().as_ref());
        let section = gtk::ShortcutsSection::builder().visible(true).build();
        for (group_title, actions) in shortcuts::SHORTCUTS {
            let group = gtk::ShortcutsGroup::builder().title(group_title).build();
            for shortcut in actions.iter() {
                let s = gtk::ShortcutsShortcut::builder()
                    .shortcut_type(gtk::ShortcutType::Accelerator)
                    .accelerator(shortcut.accel)
                    .title(shortcut.title)
                    .build();
                s.set_action_name(shortcut.action);
                group.add(&s);
            }
            section.add(&group);
        }
        window.add(&section);
        window.show_all();
    }
}

#[cfg(not(target_os = "macos"))]
fn configure() -> Result<(), Box<dyn Error>> {
    Ok(())
}

#[cfg(target_os = "macos")]
fn configure() -> Result<(), Box<dyn Error>> {
    let settings = gtk::Settings::default().ok_or("No default settings found.")?;
    settings.set_property(
        "gtk-decoration-layout",
        "close,minimize,maximize".to_value(),
    );
    Ok(())
}
