use std::collections::BTreeMap;
use std::path::Path;
use std::process::ExitCode;

use error_stack::{Result, ResultExt};

// mod data;
mod dwarf;
mod parsed;
mod util;
mod worker;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to parse uking symbols")]
    Uking,
    #[error("Failed to process DWARF")]
    Dwarf,
}

fn main() -> ExitCode {
    if let Err(e) = main_inner() {
        eprintln!("\nError: {:?}", e);
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn main_inner() -> Result<(), Error> {
    let mut uking_symbols = BTreeMap::new();
    util::read_uking_functions("botw-decomp/data/uking_functions.csv", &mut uking_symbols)
        .change_context(Error::Uking)?;
    let data_symbols =
        util::read_uking_data("botw-decomp/data/data_symbols.csv", &mut uking_symbols)
            .change_context(Error::Uking)?;
    println!("Parsed {} uking symbols", uking_symbols.len());
    let elf_path = Path::new("uking.elf");
    dwarf::extract(elf_path, "output.yaml", &mut uking_symbols, &data_symbols)
        .change_context(Error::Dwarf)?;
    Ok(())
}
