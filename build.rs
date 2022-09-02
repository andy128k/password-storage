use std::env;
use std::error::Error;
use std::path::{Path, MAIN_SEPARATOR};
use std::process::Command;

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = env::var("OUT_DIR")?;

    let out_path = Path::new(&out_dir).join("icons.gresource");
    let out_path = out_path.to_str().ok_or("Cannot build out_path")?;

    Command::new("glib-compile-resources")
        .arg(&format!("--target={}", out_path))
        .arg("icons.gresource.xml")
        .status()?;

    #[cfg(target_os = "windows")]
    winres::WindowsResource::new()
        .set_icon("icons/app-icon/password-storage.ico")
        .compile()?;

    println!("cargo:rustc-env=PATH_MAIN_SEPARATOR={}", MAIN_SEPARATOR);

    Ok(())
}
