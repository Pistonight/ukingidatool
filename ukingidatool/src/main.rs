use std::process::ExitCode;

use clap::Parser;
use error_stack::ResultExt;

mod ida_import;
mod tyyaml;

/// UKing IDA Tool
///
/// Tool for importing type and symbol information from BOTW decompile project into IDA.
#[derive(Debug, Clone, clap::Parser)]
pub struct CLI {
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(Debug, Clone, clap::Subcommand)]
pub enum Subcommand {
    /// Generate a IDA Python script to import extract data. Requires IDA Pro 7.6+
    Import(ida_import::IDAImportCLI),
}

fn main() -> ExitCode {
    let CLI { subcommand } = CLI::parse();
    match subcommand {
        Subcommand::Import(cli) => common::run(|| {
            ida_import::run_cli(cli)
                .attach_printable_lazy(|| "See `ukingidatool import --help` for more information")
        }),
    }
    // let input = "output.yaml";
    // let output = "import.py";
    // let type_pattern = "";
    // let type_only = false;
    // let options = ida_import::IDAImportOptions {
    //     input: input.into(),
    //     output: output.into(),
    //     type_pattern: type_pattern.to_string(),
    //     type_only,
    // };
    // common::run(|| ida_import::create_ida_import(&options))
}
