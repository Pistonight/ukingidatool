use std::collections::BTreeMap;
use std::path::Path;
use std::process::ExitCode;
use std::time::Instant;

use error_stack::{Result, ResultExt};
use parsed::{AddrType, AddressInfo, DataInfo, TypeYaml};
use util::ProgressPrinter;

mod dwarf;
mod parsed;
mod util;
mod uking;
mod worker;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to parse uking symbols")]
    Uking,
    #[error("Failed to process DWARF")]
    Dwarf,
    #[error("Failed to create type definitions")]
    CreateType,
    #[error("Failed to write output file")]
    WriteFile,
}

fn main() -> ExitCode {
    let start_time = Instant::now();
    if let Err(e) = main_inner() {
        eprintln!("\nError: {:?}", e);
        return ExitCode::FAILURE;
    }
    let elapsed = start_time.elapsed();
    println!("Finished in {:.02} seconds", elapsed.as_secs_f32());
    ExitCode::SUCCESS
}

fn main_inner() -> Result<(), Error> {
    let mut uking_symbols = BTreeMap::new();
    let decompiled_functions = uking::read_uking_functions("botw-decomp/data/uking_functions.csv", &mut uking_symbols)
        .change_context(Error::Uking)?;
    let data_symbols =
        uking::read_uking_data("botw-decomp/data/data_symbols.csv", &mut uking_symbols)
            .change_context(Error::Uking)?;
    println!("Parsed {} symbols, found {} decompiled functions", uking_symbols.len(), decompiled_functions.len());

    let elf_path = Path::new("uking.elf");
    let mut dwarf = dwarf::extract(elf_path, &mut uking_symbols, &decompiled_functions).change_context(Error::Dwarf)?;

    let progress = ProgressPrinter::new(uking_symbols.len(), "Add undecompiled symbols");
    for (i, (symbol, address)) in uking_symbols.into_iter().enumerate() {
        progress.print(i, format!("0x{:016x} {}", address, symbol));
        if dwarf.address.contains_key(&symbol) {
            // already processed, not possible
            panic!(
                "Symbol `{}` is already processed. This shouldn't be possible",
                symbol
            );
        }
        if data_symbols.contains(&symbol) {
            // data symbol
            dwarf.address.insert(
                symbol.clone(),
                AddressInfo {
                    uking_address: address,
                    name: symbol,
                    info: AddrType::Data(DataInfo { ty_offset: None }),
                },
            );
        } else {
            // function symbol
            dwarf.address.insert(
                symbol.clone(),
                AddressInfo {
                    uking_address: address,
                    name: symbol,
                    info: AddrType::Undecompiled,
                },
            );
        }
    }
    progress.done();

    let output_path = "output.yaml";
    // Type Output
    let type_defs = dwarf.types
        .create_defs()
        .change_context(Error::CreateType)?;

    println!("Created {} types (not including _vtbl)", type_defs.len());

    let mut type_yaml = String::new();
    type_yaml.push_str("enums:\n");
    for enum_def in type_defs.values().filter_map(|x| x.as_enum()) {
        type_yaml.push_str(&enum_def.yaml_string());
    }
    type_yaml.push_str("unions:\n");
    for union_def in type_defs.values().filter_map(|x| x.as_union()) {
        type_yaml.push_str(&union_def.yaml_string());
    }
    type_yaml.push_str("structs:\n");
    for struct_def in type_defs.values().filter_map(|x| x.as_struct()) {
        type_yaml.push_str(&struct_def.yaml_string());
    }

    // Write Output
    std::fs::write(output_path, type_yaml).change_context(Error::WriteFile)?;
    println!("Output written to {}", output_path);

    Ok(())
}
