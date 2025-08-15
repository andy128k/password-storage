pub mod algorithm;
pub mod channel;
pub mod grid_layout;
pub mod list_model;
pub mod menu_builder;
pub mod path;
pub mod string;
pub mod style;
pub mod typed_list_store;
pub mod ui;

use gtk::{gio, glib, prelude::*};
use std::rc::Rc;

pub trait PSActionEntryBuilderExt<O> {
    fn activate_async<F, R>(self, callback: F) -> Self
    where
        F: Fn(O, gio::SimpleAction, Option<glib::Variant>) -> R + 'static,
        R: std::future::Future<Output = ()>;
}

impl<O: IsA<gio::ActionMap>> PSActionEntryBuilderExt<O> for gio::ActionEntryBuilder<O> {
    fn activate_async<F, R>(self, callback: F) -> Self
    where
        F: Fn(O, gio::SimpleAction, Option<glib::Variant>) -> R + 'static,
        R: std::future::Future<Output = ()>,
    {
        let callback = Rc::new(callback);
        self.activate(move |obj, action, parameter| {
            let obj = obj.clone();
            let action = action.clone();
            let parameter = parameter.cloned();
            let callback = callback.clone();
            glib::spawn_future_local(async move {
                (callback)(obj, action, parameter).await;
            });
        })
    }
}
