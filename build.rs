fn main() {
    glib_build_tools::compile_resources(&["."], "icons.gresource.xml", "icons.gresource");

    #[cfg(target_os = "windows")]
    embed_resource::compile("./icons.rc", embed_resource::NONE)
        .manifest_required()
        .unwrap();
}
