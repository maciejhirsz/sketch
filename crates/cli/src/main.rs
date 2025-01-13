use std::io::{self, IsTerminal};
use std::process::ExitCode;
use std::{env, path::PathBuf};

use clap::{Args, Parser, Subcommand, ValueEnum};

use crate::build::build;
use crate::init::init;
use crate::serve::serve;

mod build;
mod init;
#[expect(dead_code, unused_imports)]
mod js;
mod log;
mod manifest;
mod report;
mod serve;

/// CLI tools for the Kobold framework
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    /// Use verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    /// When to use colors in output
    #[arg(long, default_value_t, value_enum, global = true)]
    color: When,
}

#[derive(Subcommand)]
enum Command {
    /// Build a kobold crate
    #[command(visible_alias = "b")]
    Build(Build),

    /// Create a new kobold crate
    Init(Init),

    /// Start a local development server
    #[command(visible_alias = "s")]
    Serve(Serve),
}

#[derive(Clone, Copy, Default, ValueEnum)]
enum When {
    #[default]
    Auto,
    Always,
    Never,
}

#[derive(Args)]
struct Build {
    /// The asset output directory
    #[arg(short, long, default_value = "dist")]
    dist: PathBuf,

    /// Build the crate in release mode
    #[arg(short, long)]
    release: bool,

    /// When to add auto-reload script to the final build
    #[arg(long, default_value_t, value_enum)]
    autoreload: When,
}

#[derive(Args)]
struct Init {
    /// Package directory, defaults to the current directory
    path: Option<PathBuf>,

    /// Set the resulting package name, defaults to the directory name
    #[arg(long)]
    name: Option<String>,
}

#[derive(Args)]
struct Serve {
    /// The development server address
    #[arg(short, long, default_value = "127.0.0.1")]
    address: String,

    /// The development server port
    #[arg(short, long, default_value_t = 3000)]
    port: u16,

    /// Watch files and directories
    #[arg(short, long)]
    watch: Vec<PathBuf>,

    #[clap(flatten)]
    build: Build,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    if cli.verbose {
        log::enable_verbose_output();
    }

    let color_allowed = env::var("NO_COLOR").map_or(true, |v| v.is_empty());
    if color_allowed {
        match cli.color {
            When::Auto if io::stdout().is_terminal() => log::enable_color_output(),
            When::Always => log::enable_color_output(),
            _ => {}
        }
    }

    let res = match cli.command {
        Command::Build(b) => build(&b),
        Command::Init(i) => init(&i),
        Command::Serve(s) => serve(&s),
    };

    match res {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            log::error!("{err}");
            ExitCode::FAILURE
        }
    }
}
