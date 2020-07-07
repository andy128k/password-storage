use gio::prelude::*;
use gio::ApplicationFlags;
use glib::clone;
use gtk::prelude::*;
use gtk::Application;
use std::cell::RefCell;
use std::rc::Rc;

use crate::cache::Cache;
use crate::config::Config;
use crate::main_window::{do_open_file, old_main, PSMainWindow};
use crate::ui::dialogs::about::about;
use crate::ui::dialogs::preferences::preferences;

struct PSApplicationPrivate {
    config: Rc<RefCell<Config>>,
    cache: Cache,
}

#[derive(Clone)]
pub struct PSApplication(gtk::Application);

pub struct PSApplicationWeak(glib::object::WeakRef<gtk::Application>);

impl glib::clone::Downgrade for PSApplication {
    type Weak = PSApplicationWeak;

    fn downgrade(&self) -> Self::Weak {
        PSApplicationWeak(glib::clone::Downgrade::downgrade(&self.0))
    }
}

impl glib::clone::Upgrade for PSApplicationWeak {
    type Strong = PSApplication;

    fn upgrade(&self) -> Option<Self::Strong> {
        glib::clone::Upgrade::upgrade(&self.0).map(PSApplication)
    }
}

impl PSApplication {
    fn create() -> Self {
        let gtk_app = Application::new(
            Some("net.andy128k.password-storage"),
            ApplicationFlags::HANDLES_OPEN,
        )
        .expect("Initialization of application failed.");
        let private = PSApplicationPrivate {
            config: Rc::new(RefCell::new(Config::load())),
            cache: Cache::load(),
        };
        unsafe {
            gtk_app.set_data("private", private);
        }
        Self(gtk_app)
    }

    fn private(&self) -> &PSApplicationPrivate {
        unsafe { self.0.get_data("private").unwrap() }
    }

    pub fn new_app() -> Self {
        let app = Self::create();

        app.0.connect_startup(move |_app| {
            crate::icons::load_icons().unwrap();
        });
        app.0.connect_activate(clone!(@weak app => move |_app| {
            app.activate();
        }));
        app.0.connect_shutdown(clone!(@weak app => move |_app| {
            let private = app.private();
            private.config.borrow().save().unwrap();
            private.cache.save().unwrap();
        }));
        app.0
            .connect_open(clone!(@weak app => move |_app, files, _hint| {
                if let Some(path) = files[0].get_path() {
                    let win = app.activate();
                    do_open_file(&win, &path);
                }
            }));

        app.0.add_action(&{
            let action = gio::SimpleAction::new("quit", None);
            action.connect_activate(clone!(@weak app => move |_, _| {
                for window in app.0.get_windows() {
                    if let Some(win) = PSMainWindow::from_window(&window) {
                        win.close();
                    }
                }
            }));
            action
        });
        app.0.add_action(&{
            let action = gio::SimpleAction::new("about", None);
            action.connect_activate(clone!(@weak app => move |_, _| {
                let win = app.0.get_active_window();
                about(win.as_ref());
            }));
            action
        });
        app.0.add_action(&{
            let action = gio::SimpleAction::new("preferences", None);
            action.connect_activate(clone!(@weak app => move |_, _| {
                let win = app.activate();
                let private = app.private();
                let config = private.config.borrow().clone();
                if let Some(new_config) = preferences(&win.window(), &config) {
                    *private.config.borrow_mut() = new_config;
                }
            }));
            action
        });
        app.0.add_action(&{
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
        app.0.add_action(&{
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
        let code = self.0.run(&argv);
        std::process::exit(code);
    }

    fn active_window(&self) -> Option<PSMainWindow> {
        self.0
            .get_active_window()
            .and_then(|w| PSMainWindow::from_window(&w))
    }

    fn activate(&self) -> PSMainWindow {
        if let Some(win) = self.active_window() {
            win
        } else {
            self.new_window()
        }
    }

    fn new_window(&self) -> PSMainWindow {
        let gtk_app = self.0.clone();
        let private = self.private();
        old_main(&gtk_app, &private.config, &private.cache)
    }
}
