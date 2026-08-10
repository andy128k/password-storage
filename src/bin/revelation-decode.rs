use clap::Parser;
use password_storage::format::revelation;
use rpassword::prompt_password;
use std::{error::Error, fs, io::BufReader, path::PathBuf};

#[derive(Parser)]
struct Opts {
    /// Input file
    input: PathBuf,

    /// Output file
    output: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();

    let password = prompt_password(format!(
        "Enter a password for a file {}: ",
        opts.input.display()
    ))?;
    println!();

    let file = fs::OpenOptions::new().read(true).open(&opts.input)?;
    let mut buffer = BufReader::new(file);

    let data = revelation::decrypt_revelation_file(&mut buffer, password.trim())?;

    fs::write(&opts.output, data.content)?;

    println!(
        "Output file: {}\nData format: {:?}\nDone",
        opts.output.display(),
        data.format
    );

    Ok(())
}
