use std::env;
use std::fs;
use std::path::Path;

use crate::log;
use crate::report::{Error, ErrorExt, Report};
use crate::Init;

pub fn init(init: &Init) -> Report<()> {
    log::creating!("kobold package");

    let path = match &init.path {
        Some(path) => path,
        None => &env::current_dir().message("failed to get current directory")?,
    };

    let cargo_path = path.join("Cargo.toml");
    if cargo_path.is_file() {
        return Err(Error::message(
            "`kobold init` cannot be run on existing Cargo packages",
        ));
    }

    let name = match &init.name {
        Some(name) => name,
        None => match path.file_name().and_then(|s| s.to_str()) {
            Some(name) => name,
            None => {
                return Err(Error::message(format!(
                    "cannot auto-detect package name from path \"{}\"; use --name to override",
                    path.display(),
                )))
            }
        },
    };

    write_file(&cargo_path, &make_cargo_toml(name))?;
    write_file(&path.join("index.html"), include_str!("../init/index.html"))?;
    write_file(&path.join("src/lib.rs"), include_str!("../init/lib.rs"))?;

    Ok(())
}

fn write_file(path: &Path, contents: &str) -> Report<()> {
    if path.is_file() {
        return Ok(());
    }

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).message("failed to create parent directories")?;
    }

    let file_name = path
        .file_name()
        .and_then(|s| s.to_str())
        .expect("file name should be valid");

    fs::write(path, contents).with_message(|| format!("failed to create `{file_name}` file"))
}

fn make_cargo_toml(name: &str) -> String {
    let template = include_str!("../init/Cargo.toml");
    let kobold_version = env!("CARGO_PKG_VERSION");

    let mut out = String::new();
    let mut key = false;
    for chunk in template.split(':') {
        if key {
            match chunk {
                "NAME" => out.push_str(name),
                "KOBOLD_VERSION" => out.push_str(kobold_version),
                _ => panic!("undefined key"),
            }
        } else {
            out.push_str(chunk);
        }

        key = !key;
    }

    out
}
