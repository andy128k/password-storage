use clap::Parser;
use password_storage::format::revelation;
use std::error::Error;
use std::fs;
use std::io::{stdin, stdout, BufReader, Write};
use std::path::PathBuf;

#[derive(Parser)]
struct Opts {
    /// Input file
    input: PathBuf,

    /// Output file
    output: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();

    print!("Enter a password for a file {}: ", opts.input.display());
    stdout().flush()?;

    let mut password = String::new();
    stdin().read_line(&mut password)?;
    println!("");

    let file = fs::OpenOptions::new().read(true).open(&opts.input)?;
    let mut buffer = BufReader::new(file);

    let data = revelation::decrypt_revelation_file(&mut buffer, &password.trim())?;

    fs::write(&opts.output, data.content)?;

    println!(
        "Output file: {}\nData format: {:?}\nDone",
        opts.output.display(),
        data.format
    );

    Ok(())
}
