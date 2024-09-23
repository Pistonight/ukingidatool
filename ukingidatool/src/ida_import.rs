use std::io::BufReader;
use std::path::PathBuf;
use std::{fs::File, num::ParseIntError};

use common::ProgressPrinter;
use error_stack::{report, Report, Result, ResultExt};

use crate::tyyaml::{Address, DataSheet};

#[derive(Debug, Clone, clap::Parser)]
pub struct IDAImportCLI {
    /// (Optional) Input path of the data file generated by `ukingidatool extract`
    ///
    /// If not specified and the current directory is a subdirectory of
    /// the botw decompile project, the input path will be set to `<botw>/build/uking-extract.yaml`
    pub input: Option<String>,
    /// Output path of the generated IDA Python script.
    ///
    /// If not specified and the current directory is a subdirectory of
    /// the botw decompile project, the output path will be set to `<botw>/build/uking-import.py`
    #[clap(short, long)]
    pub output: Option<String>,
    /// Only import symbols and types whose name contains this substring. Note
    /// that for decompiled functions, the mangled name is used.
    ///
    /// Dependent types are still imported recursively
    #[clap(short, long)]
    pub pattern: Option<String>,
    /// Only import types. Don't import address symbols
    #[clap(long)]
    pub type_only: bool,
    /// Specify the upper 8 bytes of the address. For example `-a 0x00`.
    ///
    /// The default is 0x71 to be consistent with the botw decompile project
    #[clap(short, long, default_value = "0x71", value_parser = parse_number)]
    pub address: u32,
    /// Only import names of the address symbols. Don't import any type
    ///
    /// This exists as an alternative to running the rename script in the botw decompile
    /// project. It will also import data symbols, but will not rename function arguments.
    #[clap(long, conflicts_with = "type_only", conflicts_with = "type_pattern")]
    pub name_only: bool,

    /// Assume types are already imported. The generated script will error if types are not already imported.
    ///
    /// This is useful if you have already imported with `--type-only`
    #[clap(
        long,
        conflicts_with = "name_only",
        conflicts_with = "type_only",
        conflicts_with = "type_pattern"
    )]
    pub skip_types: bool,

    /// Verbosity level
    ///
    /// `-v1` will print member info. `-v2` will print member info and renaming info
    #[clap(short, long, default_value = "0")]
    pub verbose: u32,
}

fn parse_number(s: &str) -> std::result::Result<u32, ParseIntError> {
    if let Some(s) = s.strip_prefix("0x") {
        u32::from_str_radix(s, 16)
    } else {
        s.parse()
    }
}

pub fn run_cli(cli: IDAImportCLI) -> Result<(), Error> {
    let options = IDAImportOptions::try_from(cli)?;
    create_ida_import(&options)
}

#[derive(Debug, Clone, clap::Parser)]
pub struct IDAImportOptions {
    pub input: PathBuf,
    pub output: PathBuf,
    pub pattern: String,
    pub type_only: bool,
    pub address: u32,
    pub name_only: bool,
    pub skip_types: bool,
    pub verbose: u32,
}
impl TryFrom<IDAImportCLI> for IDAImportOptions {
    type Error = Report<Error>;

    fn try_from(value: IDAImportCLI) -> Result<Self, Error> {
        let (input, output) = match (value.input, value.output) {
            (Some(input), Some(output)) => (input.into(), output.into()),
            (i, o) => {
                let botw = common::find_botw()
                    .ok_or(Error::BotwNotFound)
                    .attach_printable("Please specify --input and --output manually")?;
                let input = i.map_or_else(
                    || botw.join("build").join("uking-extract.yaml"),
                    PathBuf::from,
                );
                let output =
                    o.map_or_else(|| botw.join("build").join("uking-import.py"), PathBuf::from);
                (input, output)
            }
        };
        if !input.exists() {
            return Err(report!(Error::DataNotFound))
                .attach_printable(format!("Trying to read file: {}", input.display()))
                .attach_printable(
                    "If you haven't already, run `ukingidatool extract` to generate the data file.",
                )
                .attach_printable("Or, specify the input file manually with `--input`.");
        }
        Ok(Self {
            input,
            output,
            pattern: value.pattern.unwrap_or_default(),
            type_only: value.type_only,
            address: value.address,
            name_only: value.name_only,
            skip_types: value.skip_types,
            verbose: value.verbose,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Botw decompile project not found.")]
    BotwNotFound,
    #[error("Data file not found!")]
    DataNotFound,
    #[error("Failed to read input file")]
    ReadFile,
    #[error("Failed to parse input file")]
    ParseFile,
    #[error("Failed to write output file")]
    WriteFile,
}

pub fn create_ida_import(options: &IDAImportOptions) -> Result<(), Error> {
    let input_path_str = options.input.display().to_string();
    println!("Reading data from {}", input_path_str);
    let file = File::open(&options.input).change_context(Error::ReadFile)?;
    let mut reader = BufReader::new(file);
    let data_sheet: DataSheet =
        serde_yaml::from_reader(&mut reader).change_context(Error::ParseFile)?;

    let mut output = String::new();

    output.push_str(include_str!("../ida_header.py"));
    output.push_str("#//////////// Options ////////////\n");
    output.push_str(&format!("# Data file: {}\n", options.input.display()));
    if !options.pattern.is_empty() {
        output.push_str(&format!("# Pattern: {}\n", options.pattern));
    }
    if options.type_only {
        output.push_str("# Type only\n");
    }
    if options.name_only {
        output.push_str("# Name only\n");
    }
    if options.skip_types {
        output.push_str("# Skip types\n");
    }
    output.push_str(&format!("# Upper Address: 0x{:08X}\n", options.address));
    if options.verbose > 0 {
        output.push_str(&format!("# Verbosity {}\n", options.verbose));
    }
    output.push_str("#//////////// Options ////////////\n");
    output.push_str(include_str!("../ida.py"));

    output.push_str(&format!("        _set_verbose({})\n", options.verbose));
    if options.skip_types {
        output.push_str("        ti.skip()\n");
    }
    output.push_str(&format!(
        "        ai.set_upper(0x{:08X})\n",
        options.address
    ));

    if !options.name_only && !options.skip_types {
        let progress = ProgressPrinter::new(data_sheet.enums.len(), "Load enums");
        for (i, def) in data_sheet.enums.iter().enumerate() {
            progress.print(i, &def.name);
            for line in def.emit_python() {
                output.push_str(&format!("        {}\n", line));
            }
        }
        progress.done();

        let progress = ProgressPrinter::new(data_sheet.unions.len(), "Load unions");
        for (i, def) in data_sheet.unions.iter().enumerate() {
            progress.print(i, &def.name);
            for line in def.emit_python() {
                output.push_str(&format!("        {}\n", line));
            }
        }
        progress.done();

        let progress = ProgressPrinter::new(data_sheet.structs.len(), "Load structs");
        for (i, def) in data_sheet.structs.iter().enumerate() {
            progress.print(i, &def.name);
            for line in def.emit_python() {
                output.push_str(&format!("        {}\n", line));
            }
        }
        progress.done();

        output.push_str(&format!("        ti.run_import(\"{}\")\n", options.pattern));
    }

    if !options.type_only {
        let progress = ProgressPrinter::new(data_sheet.addresses.len(), "Load symbols");
        for (i, (offset, def)) in data_sheet.addresses.iter().enumerate() {
            match def {
                Address::Function(func) => {
                    progress.print(i, &func.name);
                    for line in func.emit_python(offset) {
                        output.push_str(&format!("        {}\n", line));
                    }
                }
                Address::Data(data) => {
                    progress.print(i, &data.name);
                    let s = data.emit_python(offset);
                    output.push_str(&format!("        {}\n", s));
                }
            }
        }
        progress.done();

        let name_only = if options.name_only { "True" } else { "False" };
        output.push_str(&format!(
            "        ai.run_import({}, \"{}\")\n",
            name_only, options.pattern
        ));
    }

    output.push_str(include_str!("../ida_footer.py"));
    common::ensure_parent_exists(&options.output).change_context(Error::WriteFile)?;

    let output_path_str = options.output.display().to_string();
    std::fs::write(&options.output, output).change_context(Error::WriteFile)?;
    println!("Script saved to {}", output_path_str);
    println!(
        "Please make sure to run the script AFTER auto-analysis is complete in a fresh database."
    );

    Ok(())
}
