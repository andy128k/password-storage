use crate::compat::init::init;
use crate::main_window::PSMainWindow;
use crate::ui::dialogs::about::about;
use crate::ui::dialogs::preferences::preferences;
use crate::ui::dialogs::shortcuts::shortcuts_window;
use crate::utils::path::path_from_bytes;
use gtk::{gio, glib, prelude::*, subclass::prelude::*};

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
            gio::resources_register_include!("icons.gresource").expect("icons are registered");
            self.parent_startup();
            init();

            self.cache.load();

            for (_group_title, actions) in crate::shortcuts::SHORTCUTS {
                for crate::shortcuts::Shortcut { action, accel, .. } in actions.iter() {
                    if let Some(action) = action {
                        self.obj().set_accels_for_action(action, &[accel]);
                    }
                }
            }

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
            glib::spawn_future_local(glib::clone!(
                #[weak(rename_to = imp)]
                self,
                async move {
                    imp.on_open(&files, &hint).await;
                }
            ));
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
            PSMainWindow::new(self.obj().upcast_ref(), &self.config, &self.cache)
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
        @implements gio::ActionMap, gio::ActionGroup;
}

impl Default for PSApplication {
    fn default() -> Self {
        glib::Object::builder()
            .property("application-id", APPLICATION_ID)
            .property("flags", gio::ApplicationFlags::HANDLES_OPEN)
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
        if let Some(new_config) = preferences(win.upcast_ref(), &config).await {
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
        let filename = path_from_bytes(buffer);
        let win = self.imp().activate_main_window();
        win.do_open_file(&filename).await;
    }

    #[action(name = "shortcuts")]
    fn action_shortcuts(&self) {
        let window = shortcuts_window(self.active_window().as_ref(), crate::shortcuts::SHORTCUTS);
        window.present();
    }
}
