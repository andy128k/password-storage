use crate::cache::Cache;
use crate::config::ConfigService;
use crate::gtk_prelude::*;
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

impl ObjectImpl for PSApplication {
    fn constructed(&self, app: &Self::Type) {
        self.parent_constructed(app);
        app.connect_startup(|app| app.on_startup());
        app.connect_activate(|app| app.on_activate());
        app.connect_shutdown(|app| app.on_shutdown());
        app.connect_open(|app, files, hint| {
            let app = app.clone();
            let files = files.to_owned();
            let hint = hint.to_owned();
            glib::MainContext::default().spawn_local(async move {
                app.on_open(&files, &hint).await;
            });
        });
        app.register_actions(app);
    }
}

impl ApplicationImpl for PSApplication {}
impl GtkApplicationImpl for PSApplication {}
