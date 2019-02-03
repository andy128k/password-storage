use std::error::Error;
use std::process::Command;
use std::env;

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = env::var("OUT_DIR")?;

    Command::new("glib-compile-resources")
        .arg(&format!("--target={}/icons.gresource", out_dir))
        .arg("icons.gresource.xml")
        .status()?;

    Ok(())
}
