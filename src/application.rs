use std::rc::Rc;
use gio::prelude::*;
use gio::ApplicationFlags;
use gtk::{Application, ClipboardExt};

use config::Config;
use main_window::old_main;
use utils::clipboard::get_clipboard;

struct PSApplicationPrivate {
    gtk_app: Application,
    config: Config,
}

#[derive(Clone)]
pub struct PSApplication(Rc<::debug_cell::RefCell<PSApplicationPrivate>>);

impl PSApplication {
    pub fn new() -> Self {
        let gtk_app = Application::new("net.andy128k.password-storage", ApplicationFlags::empty())
            .expect("Initialization of application failed.");

        let private = Rc::new(::debug_cell::RefCell::new(PSApplicationPrivate {
            gtk_app,
            config: Config::load(),
        }));

        {
            private.borrow().gtk_app.connect_startup(move |_app| {
                ::icons::load_icons().unwrap();
            });
        }
        {
            let private_weak = Rc::downgrade(&private);
            private.borrow().gtk_app.connect_activate(move |_app| {
                if let Some(private) = private_weak.upgrade() {
                    let app = PSApplication(private);
                    old_main(&app);
                }
            });
        }
        {
            let private_weak = Rc::downgrade(&private);
            private.borrow().gtk_app.connect_shutdown(move |_app| {
                if let Some(private) = private_weak.upgrade() {
                    private.borrow().config.save().unwrap();
                }
            });
        }

        PSApplication(private)
    }

    pub fn get_application(&self) -> Application {
        self.0.borrow().gtk_app.clone()
    }

    pub fn get_config(&self) -> Config {
        self.0.borrow().config.clone()
    }

    pub fn set_config(&self, new_config: Config) {
        self.0.borrow_mut().config = new_config;
    }

    pub fn run(&self) {
        let argv: Vec<String> = ::std::env::args().collect();
        let gtk_app = self.0.borrow().gtk_app.clone();
        let code = gtk_app.run(&argv);

        ::std::process::exit(code);
    }

    pub fn quit(&self) {
        self.0.borrow().gtk_app.quit();
        get_clipboard().clear();
    }
}
