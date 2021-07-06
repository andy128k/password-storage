use crate::cache::Cache;
use crate::config::Config;
use crate::main_window::PSMainWindow;
use crate::ui::dialogs::about::about;
use crate::ui::dialogs::preferences::preferences;
use crate::ui::menu::create_menu_bar;
use gtk::{
    gio,
    glib::{self, clone},
    prelude::*,
    subclass::prelude::*,
};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Default)]
pub struct PSApplicationInner {
    config: Rc<RefCell<Config>>,
    cache: Cache,
}

#[glib::object_subclass]
impl ObjectSubclass for PSApplicationInner {
    const NAME: &'static str = "PSApplication";
    type Type = PSApplication;
    type ParentType = gtk::Application;
}

impl ObjectImpl for PSApplicationInner {
    fn constructed(&self, app: &Self::Type) {
        self.parent_constructed(app);

        *self.config.borrow_mut() = Config::load();
        self.cache.load();

        app.connect_startup(move |app| {
            crate::icons::load_icons().unwrap();
            app.set_menubar(Some(&create_menu_bar()));
        });
        app.connect_activate(clone!(@weak app => move |_app| {
            app.activate_main_window();
        }));
        app.connect_shutdown(clone!(@weak app => move |_app| {
            let private = PSApplicationInner::from_instance(&app);

            private.config.borrow().save().unwrap();
            private.cache.save().unwrap();
        }));
        app.connect_open(clone!(@weak app => move |_app, files, _hint| {
            if let Some(path) = files[0].path() {
                let win = app.activate_main_window();
                win.do_open_file(&path);
            }
        }));

        app.add_action(&{
            let action = gio::SimpleAction::new("quit", None);
            action.connect_activate(clone!(@weak app => move |_, _| app.action_quit()));
            action
        });
        app.add_action(&{
            let action = gio::SimpleAction::new("about", None);
            action.connect_activate(clone!(@weak app => move |_, _| app.action_about()));
            action
        });
        app.add_action(&{
            let action = gio::SimpleAction::new("preferences", None);
            action.connect_activate(clone!(@weak app => move |_, _| app.action_preferences()));
            action
        });
        app.add_action(&{
            let action = gio::SimpleAction::new("new", None);
            action.connect_activate(clone!(@weak app => move |_, _| app.action_new()));
            action
        });
        app.add_action(&{
            let action = gio::SimpleAction::new("open", None);
            action.connect_activate(clone!(@weak app => move |_, _| app.action_open()));
            action
        });
    }
}

impl ApplicationImpl for PSApplicationInner {}
impl GtkApplicationImpl for PSApplicationInner {}

glib::wrapper! {
    pub struct PSApplication(ObjectSubclass<PSApplicationInner>)
        @extends gtk::Application, gio::Application, gio::ActionMap;
}

impl PSApplication {
    pub fn new() -> Self {
        glib::Object::new(&[
            ("application-id", &"net.andy128k.password-storage"),
            ("flags", &gio::ApplicationFlags::HANDLES_OPEN),
        ])
        .expect("Application is created")
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
        let private = PSApplicationInner::from_instance(self);
        let window = PSMainWindow::new(&self.clone().upcast(), &private.config, &private.cache);
        window.show_all();
        window
    }
}

// actions
impl PSApplication {
    fn action_quit(&self) {
        for window in self.windows() {
            if let Some(win) = PSMainWindow::from_window(&window) {
                win.close();
            }
        }
    }

    fn action_about(&self) {
        let win = self.active_window();
        about(win.as_ref());
    }

    fn action_preferences(&self) {
        let win = self.activate_main_window();
        let private = PSApplicationInner::from_instance(self);
        let config = private.config.borrow().clone();
        if let Some(new_config) = preferences(&win.clone().upcast(), &config) {
            *private.config.borrow_mut() = new_config;
        }
    }

    fn action_new(&self) {
        if let Some(win) = self.active_main_window() {
            win.new_file();
        } else {
            self.new_window();
        }
    }

    fn action_open(&self) {
        let win = self.activate_main_window();
        win.open_file();
    }
}
