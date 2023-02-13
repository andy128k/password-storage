fn main() {
    glib_build_tools::compile_resources(&["."], "icons.gresource.xml", "icons.gresource");

    #[cfg(target_os = "windows")]
    winres::WindowsResource::new()
        .set_icon("icons/app-icon/password-storage.ico")
        .compile()
        .unwrap();
}
