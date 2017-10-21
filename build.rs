use std::process::Command;
use std::env;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    Command::new("glib-compile-resources")
        .arg(&format!("--target={}/icons.gresource", out_dir))
        .arg("icons.gresource.xml")
        .status()
        .unwrap();
}
