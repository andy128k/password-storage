use std::rc::Rc;
use std::cell::RefCell;
use gio::prelude::*;
use gio::ApplicationFlags;
use glib::clone;
use gtk::prelude::*;
use gtk::{Application};

use crate::ptr::*;
use crate::config::Config;
use crate::cache::Cache;
use crate::main_window::{PSMainWindow, old_main, do_open_file};
use crate::ui::dialogs::about::about;
use crate::ui::dialogs::preferences::preferences;

pub struct PSApplicationPrivate {
    gtk_app: Application,
    config: Rc<RefCell<Config>>,
    cache: Cache,
}

pub type PSApplication = SharedPtr<PSApplicationPrivate>;

impl PSApplication {
    pub fn new_app() -> Self {
        let gtk_app = Application::new(Some("net.andy128k.password-storage"), ApplicationFlags::HANDLES_OPEN)
            .expect("Initialization of application failed.");
        let app = PSApplication::from_private(PSApplicationPrivate {
            gtk_app: gtk_app.clone(),
            config: Rc::new(RefCell::new(Config::load())),
            cache: Cache::load(),
        });
        gtk_app.connect_startup(move |_gtk_app| {
            crate::icons::load_icons().unwrap();
        });
        gtk_app.connect_activate(clone!(@weak app => move |_gtk_app| {
            app.activate();
        }));
        gtk_app.connect_shutdown(clone!(@weak app => move |_gtk_app| {
            app.borrow().config.borrow().save().unwrap();
            app.borrow().cache.save().unwrap();
        }));
        gtk_app.connect_open(clone!(@weak app => move |_gtk_app, files, _hint| {
            if let Some(path) = files[0].get_path() {
                let win = app.activate();
                do_open_file(&win, &path);
            }
        }));

        gtk_app.add_action(&{
            let action = gio::SimpleAction::new("quit", None);
            action.connect_activate(clone!(@weak gtk_app => move |_, _| {
                for window in gtk_app.get_windows() {
                    if let Some(win) = PSMainWindow::from_window(&window) {
                        win.close();
                    }
                }
            }));
            action
        });
        gtk_app.add_action(&{
            let action = gio::SimpleAction::new("about", None);
            action.connect_activate(clone!(@weak gtk_app => move |_, _| {
                let win = gtk_app.get_active_window();
                about(win.as_ref());
            }));
            action
        });
        gtk_app.add_action(&{
            let action = gio::SimpleAction::new("preferences", None);
            action.connect_activate(clone!(@weak app => move |_, _| {
                let win = app.activate();
                let config = app.borrow().config.borrow().clone();
                if let Some(new_config) = preferences(&win.window(), &config) {
                    *app.borrow().config.borrow_mut() = new_config;
                }
            }));
            action
        });
        gtk_app.add_action(&{
            let action = gio::SimpleAction::new("new", None);
            action.connect_activate(clone!(@weak app => move |_, _| {
                if let Some(win) = app.active_window() {
                    win.new_file();
                } else {
                    app.new_window();
                }
            }));
            action
        });
        gtk_app.add_action(&{
            let action = gio::SimpleAction::new("open", None);
            action.connect_activate(clone!(@weak app => move |_, _| {
                let win = app.activate();
                win.open_file();
            }));
            action
        });

        app
    }

    pub fn run(&self) {
        let argv: Vec<String> = std::env::args().collect();
        let gtk_app = self.borrow().gtk_app.clone();
        let code = gtk_app.run(&argv);

        std::process::exit(code);
    }

    fn active_window(&self) -> Option<PSMainWindow> {
        self.borrow().gtk_app.get_active_window().and_then(|w| PSMainWindow::from_window(&w))
    }

    fn activate(&self) -> PSMainWindow {
        if let Some(win) = self.active_window() {
            win
        } else {
            self.new_window()
        }
    }

    fn new_window(&self) -> PSMainWindow {
        let gtk_app = self.borrow().gtk_app.clone();
        let config = self.borrow().config.clone();
        let cache = self.borrow().cache.clone();
        old_main(&gtk_app, &config, &cache)
    }
}
