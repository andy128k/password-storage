use crate::gtk_prelude::*;
use crate::main_window::PSMainWindow;
use crate::ui::dialogs::about::about;
use crate::ui::dialogs::preferences::preferences;
use os_str_bytes::OsStringBytes;
use std::error::Error;
use std::path::PathBuf;

mod shortcuts;

const APPLICATION_ID: &str = "dev.andy128k.password-storage";

mod imp {
    use super::*;
    use crate::cache::Cache;
    use crate::config::ConfigService;
    use std::rc::Rc;

    #[derive(Default)]
    pub struct PSApplication {
        pub config: Rc<ConfigService>,
        pub cache: Cache,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for PSApplication {
        const NAME: &'static str = "PSApplication";
        type Type = super::PSApplication;
        type ParentType = gtk::Application;
    }

    impl ObjectImpl for PSApplication {}

    impl ApplicationImpl for PSApplication {
        fn startup(&self) {
            self.parent_startup();

            self.cache.load();

            if let Err(error) = configure() {
                eprintln!("Failed to configure global settings: {}.", error);
            }

            for (_group_title, actions) in shortcuts::SHORTCUTS {
                for shortcuts::Shortcut { action, accel, .. } in actions.iter() {
                    if let Some(action) = action {
                        self.obj().set_accels_for_action(action, &[accel]);
                    }
                }
            }

            crate::icons::load_icons().unwrap();

            let app = self.obj();
            app.register_actions(&*app);
        }

        fn shutdown(&self) {
            self.cache.save().unwrap();
            self.parent_shutdown();
        }

        fn activate(&self) {
            self.activate_main_window();
        }

        fn open(&self, files: &[gio::File], hint: &str) {
            self.activate_main_window();

            let files = files.to_owned();
            let hint = hint.to_owned();
            glib::MainContext::default().spawn_local(clone!(@weak self as imp => async move {
                imp.on_open(&files, &hint).await;
            }));
        }
    }

    impl GtkApplicationImpl for PSApplication {}

    impl PSApplication {
        pub fn active_main_window(&self) -> Option<PSMainWindow> {
            self.obj().active_window()?.downcast::<PSMainWindow>().ok()
        }

        pub fn activate_main_window(&self) -> PSMainWindow {
            if let Some(win) = self.active_main_window() {
                win
            } else {
                self.new_window()
            }
        }

        pub fn new_window(&self) -> PSMainWindow {
            PSMainWindow::new(&self.obj().upcast_ref(), &self.config, &self.cache)
        }

        async fn on_open(&self, files: &[gio::File], _hint: &str) {
            if let Some(path) = files[0].path() {
                let win = self.activate_main_window();
                win.do_open_file(&path).await;
            }
        }
    }
}

glib::wrapper! {
    pub struct PSApplication(ObjectSubclass<imp::PSApplication>)
        @extends gtk::Application, gio::Application,
        @implements gio::ActionMap;
}

impl Default for PSApplication {
    fn default() -> Self {
        glib::Object::builder()
            .property("application-id", APPLICATION_ID)
            .property("flags", &gio::ApplicationFlags::HANDLES_OPEN)
            .build()
    }
}

#[awesome_glib::actions]
impl PSApplication {
    fn quit(&self) {
        for window in self.windows() {
            window.close();
        }
    }

    async fn about(&self) {
        let win = self.active_window();
        about(win.as_ref()).await;
    }

    async fn preferences(&self) {
        let win = self.imp().activate_main_window();
        let config = self.imp().config.get();
        if let Some(new_config) = preferences(&win.clone().upcast(), &config).await {
            self.imp().config.set(new_config);
        }
    }

    #[action(name = "new")]
    async fn new_file(&self) {
        if let Some(win) = self.imp().active_main_window() {
            win.new_file().await;
        } else {
            self.imp().new_window();
        }
    }

    #[action(name = "open")]
    async fn open_file(&self) {
        let win = self.imp().activate_main_window();
        win.open_file().await;
    }

    #[action(name = "open-file")]
    async fn open_file_by_name(&self, buffer: Vec<u8>) {
        let filename = PathBuf::assert_from_raw_vec(buffer);
        let win = self.imp().activate_main_window();
        win.do_open_file(&filename).await;
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
                group.append(&s);
            }
            section.append(&group);
        }
        window.set_child(Some(&section));
        window.show();
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::test_gtk_init;

    #[test]
    fn test_app_create() {
        test_gtk_init();

        let app = PSApplication::default();
        app.quit();
    }
}
