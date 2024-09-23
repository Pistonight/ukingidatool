use std::process::ExitCode;

use clap::Parser;
use error_stack::ResultExt;

mod extract;
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
    /// Extract data types from DWARF info from the botw decompile project
    Extract(extract::ExtractCLI),
    /// Generate a IDA Python script to import extract data. Requires IDA Pro 7.6+
    Import(ida_import::IDAImportCLI),
}

fn main() -> ExitCode {
    let CLI { subcommand } = CLI::parse();
    match subcommand {
        Subcommand::Extract(cli) => common::run(|| {
            extract::run_cli(cli)
                .attach_printable_lazy(|| "See `ukingidatool extract --help` for more information")
        }),
        Subcommand::Import(cli) => common::run(|| {
            ida_import::run_cli(cli)
                .attach_printable_lazy(|| "See `ukingidatool import --help` for more information")
        }),
    }
}
