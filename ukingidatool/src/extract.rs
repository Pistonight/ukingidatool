use std::path::PathBuf;

use error_stack::{Report, Result, ResultExt};
#[derive(Debug, Clone, clap::Parser)]
pub struct ExtractCLI {
    /// (Optional) Input path of the ELF file built from botw decompile project.
    ///
    /// If not specified and the current directory is a subdirectory of
    /// the botw decompile project, the input will be set to `<botw>/build/uking`
    pub elf: Option<String>,
    /// Output path of the generated data file.
    ///
    /// If not specified and the current directory is a subdirectory of
    /// the botw decompile project, it will be set to `<botw>/build/uking-extract.yaml`
    #[clap(short, long)]
    pub output: Option<String>,
    /// Path to uking_functions.csv
    ///
    /// If not specified and the current directory is a subdirectory of
    /// the botw decompile project, it will be set to `<botw>/data/uking_functions.csv`
    #[clap(long)]
    pub func: Option<String>,
    /// Path to uking_data.csv
    ///
    /// If not specified and the current directory is a subdirectory of
    /// the botw decompile project, it will be set to `<botw>/data/data_symbols.csv`
    #[clap(long)]
    pub data: Option<String>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Botw decompile project not found.")]
    BotwNotFound,
    #[error("Failed to extract")]
    Extract,
}

impl TryInto<uking_extract::Options> for ExtractCLI {
    type Error = Report<Error>;

    fn try_into(self) -> Result<uking_extract::Options, Error> {
        let (e, o, f, d) = match (self.elf, self.output, self.func, self.data) {
            (Some(e), Some(o), Some(f), Some(d)) => (e.into(), o.into(), f.into(), d.into()),
            (e, o, f, d) => {
                let botw = common::find_botw()
                    .ok_or(Error::BotwNotFound)
                    .attach_printable(
                        "Please run inside botw decompile project or specify paths manually",
                    )?;
                let e = e.map_or_else(|| botw.join("build").join("uking"), PathBuf::from);
                let o = o.map_or_else(
                    || botw.join("build").join("uking-extract.yaml"),
                    PathBuf::from,
                );
                let f = f.map_or_else(
                    || botw.join("data").join("uking_functions.csv"),
                    PathBuf::from,
                );
                let d = d.map_or_else(|| botw.join("data").join("data_symbols.csv"), PathBuf::from);
                (e, o, f, d)
            }
        };
        Ok(uking_extract::Options {
            elf: e,
            output: o,
            func: f,
            data: d,
        })
    }
}

pub fn run_cli(cli: ExtractCLI) -> Result<(), Error> {
    let options = cli.try_into()?;
    uking_extract::extract(&options).change_context(Error::Extract)
}
