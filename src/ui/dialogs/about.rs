use crate::gtk_prelude::*;
use crate::version::VERSION;

pub async fn about(parent: Option<&gtk::Window>) {
    let dlg = gtk::AboutDialog::builder()
        .modal(true)
        .authors(vec!["Andrey Kutejko <andy128k@gmail.com>".to_string()])
        .copyright("Copyright 2009-2023, Andrey Kutejko")
        .license_type(gtk::License::Lgpl30)
        .logo_icon_name("password-storage")
        .icon_name("password-storage")
        .program_name("PasswordStorage")
        .version(VERSION)
        .website("https://password-storage.andy128k.dev")
        .build();
    dlg.set_transient_for(parent);
    dlg.show();
}
