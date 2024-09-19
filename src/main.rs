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
    #[error("Failed to create symbol definitions")]
    CreateSymbol,
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
    // Parse symbol listing
    let mut uking_symbols = BTreeMap::new();
    let decompiled_functions = uking::read_uking_functions("botw-decomp/data/uking_functions.csv", &mut uking_symbols)
        .change_context(Error::Uking)?;
    let data_symbols =
        uking::read_uking_data("botw-decomp/data/data_symbols.csv", &mut uking_symbols)
            .change_context(Error::Uking)?;
    println!("Parsed {} symbols, found {} decompiled functions", uking_symbols.len(), decompiled_functions.len());

    // Parse DWARF
    let elf_path = Path::new("uking.elf");
    let mut dwarf = dwarf::extract(elf_path, &mut uking_symbols, &decompiled_functions).change_context(Error::Dwarf)?;

    // Type Output
    let type_defs = dwarf.types
        .create_defs()
        .change_context(Error::CreateType)?;

    // Add remaining undecompiled symbols
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

    // statistics
    let mut struct_count = 0;
    let mut union_count = 0;
    let mut enum_count = 0;
    let mut func_count = 0;
    let mut data_count = 0;

    // Func/Data Output
    let progress = ProgressPrinter::new(dwarf.address.len(), "Create symbol definitions");
    let mut symbols = BTreeMap::new();
    for (i, (name, info)) in dwarf.address.into_iter().enumerate() {
        progress.print(i, &name);
        let info =
        info.into_def(&dwarf.types, &type_defs)
            .change_context(Error::CreateSymbol)?;
        if info.is_func {
            func_count += 1;
        } else {
            data_count += 1;
        }
        symbols.insert(name, info);
    }
    progress.done();

    let output_path = "output.yaml";

    let mut type_yaml = String::new();
    type_yaml.push_str("enums:\n");
    for enum_def in type_defs.values().filter_map(|x| x.as_enum()) {
        enum_count += 1;
        type_yaml.push_str(&enum_def.yaml_string());
    }
    type_yaml.push_str("unions:\n");
    for union_def in type_defs.values().filter_map(|x| x.as_union()) {
        union_count += 1;
        type_yaml.push_str(&union_def.yaml_string());
    }
    type_yaml.push_str("structs:\n");
    for struct_def in type_defs.values().filter_map(|x| x.as_struct()) {
        struct_count += 1;
        type_yaml.push_str(&struct_def.yaml_string());
    }
    type_yaml.push_str("addresses:\n");
    let mut address_defs = symbols.into_values().collect::<Vec<_>>();
    address_defs.sort_by_key(|x| x.uking_address);
    for address_def in address_defs {
        type_yaml.push_str(&address_def.yaml_string());
    }
    println!("Extracted:");
    println!("  structs: {}", struct_count);
    println!("  unions: {}", union_count);
    println!("  enums: {}", enum_count);
    println!("  functions: {}", func_count);
    println!("  data: {}", data_count);

    // Write Output
    std::fs::write(output_path, type_yaml).change_context(Error::WriteFile)?;
    println!("Output written to {}", output_path);

    Ok(())
}
