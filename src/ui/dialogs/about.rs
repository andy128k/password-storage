use crate::version::VERSION;
use gtk::prelude::*;

pub fn about(parent: Option<&gtk::Window>) {
    let dlg = gtk::AboutDialogBuilder::new()
        .window_position(gtk::WindowPosition::CenterOnParent)
        .authors(vec!["Andrey Kutejko <andy128k@gmail.com>".to_string()])
        .copyright("Copyright 2009-2020, Andrey Kutejko")
        .license_type(gtk::License::Lgpl30)
        .logo_icon_name("password-storage")
        .icon_name("password-storage")
        .program_name("PasswordStorage")
        .version(VERSION)
        .website("http://andy128k.github.com/password-storage")
        .build();
    dlg.set_transient_for(parent);
    dlg.run();
    dlg.close();
}
