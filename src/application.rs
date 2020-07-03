use gio::prelude::*;
use gio::ApplicationFlags;
use glib::clone;
use gtk::prelude::*;
use gtk::{Application};

use crate::ptr::*;
use crate::config::Config;
use crate::cache::Cache;
use crate::main_window::{PSMainWindow, old_main, do_open_file};

pub struct PSApplicationPrivate {
    gtk_app: Application,
    config: Config,
    cache: Cache,
}

pub type PSApplication = SharedPtr<PSApplicationPrivate>;

impl PSApplication {
    pub fn new_app() -> Self {
        let gtk_app = Application::new(Some("net.andy128k.password-storage"), ApplicationFlags::HANDLES_OPEN)
            .expect("Initialization of application failed.");
        let app = PSApplication::from_private(PSApplicationPrivate {
            gtk_app: gtk_app.clone(),
            config: Config::load(),
            cache: Cache::load(),
        });
        gtk_app.connect_startup(move |_gtk_app| {
            crate::icons::load_icons().unwrap();
        });
        gtk_app.connect_activate(clone!(@weak app => move |_gtk_app| {
            app.activate();
        }));
        gtk_app.connect_shutdown(clone!(@weak app => move |_gtk_app| {
            app.borrow().config.save().unwrap();
            app.borrow().cache.save().unwrap();
        }));
        gtk_app.connect_open(clone!(@weak app => move |_gtk_app, files, _hint| {
            if let Some(path) = files[0].get_path() {
                let win = app.activate();
                do_open_file(&win, &path);
            }
        }));
        app
    }

    pub fn get_application(&self) -> Application {
        self.borrow().gtk_app.clone()
    }

    pub fn get_config(&self) -> Config {
        self.borrow().config.clone()
    }

    pub fn set_config(&self, new_config: Config) {
        self.borrow_mut().config = new_config;
    }

    pub fn get_cache(&self) -> Cache {
        self.borrow().cache.clone()
    }

    pub fn run(&self) {
        let argv: Vec<String> = std::env::args().collect();
        let gtk_app = self.borrow().gtk_app.clone();
        let code = gtk_app.run(&argv);

        std::process::exit(code);
    }

    fn activate(&self) -> PSMainWindow {
        if let Some(win) = self.borrow().gtk_app.get_active_window().and_then(|w| PSMainWindow::from_window(w)) {
            win
        } else {
            old_main(self)
        }
    }
}
