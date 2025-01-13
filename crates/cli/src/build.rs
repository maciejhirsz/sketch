use std::borrow::Cow;
use std::fmt::{self, Debug, Display};
use std::fs;
use std::io;
use std::path::{absolute, Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use leb128::write::unsigned as leb128_write;
use lol_html::html_content::{ContentType, Element};
use lol_html::{element, rewrite_str, RewriteStrSettings};
use wasmparser::BinaryReaderError;

use crate::log;
use crate::manifest::{manifest, Manifest};
use crate::report::{Error, ErrorExt, Report};
use crate::{Build, When};

pub fn build(b: &Build) -> Report<()> {
    let Manifest {
        crate_name,
        crate_version,
        mut target,
    } = manifest()?;

    log::building!("{crate_name} v{crate_version}");

    build_wasm(b.release)?;

    target.push("wasm32-unknown-unknown");
    target.push(if b.release { "release" } else { "debug" });
    target.push(&crate_name);
    target.set_extension("wasm");

    if !target.exists() {
        return Err(Error::message(format!(
            "couldn't find compiled .wasm: {}",
            target.display(),
        )));
    }

    let start = Instant::now();

    run_wasm_bindgen(&target, &b.dist)?;

    let mut wasm = b.dist.join(format!("{crate_name}_bg"));
    wasm.set_extension("wasm");

    if b.release {
        optimize_wasm(&wasm)?;
    }

    let elapsed = start.elapsed();
    let wasm_path = absolute(&wasm).message("failed to get absolute path")?;
    log::optimized!("wasm `{}` in {elapsed:.2?}", wasm_path.display());

    let mut js = b.dist.join(&crate_name);
    js.set_extension("js");

    mangle_wasm(&wasm, &js)?;

    let snippets_dir = b.dist.join("snippets");
    let snippets = read_file_paths(&snippets_dir)
        .with_message(|| format!("failed to read {} directory", snippets_dir.display()))?;

    let index = b.dist.join("index.html");
    let paths = Paths {
        dist: Dist(&b.dist),
        snippets: &snippets,
        wasm: &wasm,
        js: &js,
        index: &index,
    };

    make_index_html(MakeIndex {
        orig_index: Path::new("index.html"),
        paths,
        embed_autoreload_script: match b.autoreload {
            When::Auto => !b.release,
            When::Always => true,
            When::Never => false,
        },
    })?;

    Ok(())
}

fn build_wasm(release: bool) -> Report<()> {
    let mut cargo = Command::new("cargo");
    cargo.args(["build", "--target=wasm32-unknown-unknown"]);

    if release {
        cargo.arg("--release");
    }

    let status = cargo
        .spawn()
        .message("failed to run cargo")?
        .wait()
        .message("failed to build cargo crate")?;

    if status.success() {
        Ok(())
    } else {
        Err(Error::message("failed to build cargo crate"))
    }
}

fn run_wasm_bindgen(target: &Path, dist: &Path) -> Report<()> {
    let out = Command::new("wasm-bindgen")
        .arg(target)
        .arg("--out-dir")
        .arg(dist)
        .args(["--target=web", "--no-typescript"])
        .output()
        .message("failed to run wasm-bindgen")?;

    if out.status.success() {
        Ok(())
    } else {
        let err = String::from_utf8_lossy(&out.stderr);
        Err(Error::message(format!("failed to run wasm-bindgen\n{err}")))
    }
}

fn optimize_wasm(file: &Path) -> Report<()> {
    Command::new("wasm-opt")
        .arg("-Os")
        .arg(file)
        .arg("-o")
        .arg(file)
        .args(["--enable-simd", "--low-memory-unused"])
        .spawn()
        .message("failed to run wasm-opt")?
        .wait()
        .message("failed to optimize wasm")?;

    Ok(())
}

fn mangle_wasm(wasm: &Path, js: &Path) -> Report<()> {
    let wasm_bytes =
        fs::read(wasm).with_message(|| format!("failed to read {}", wasm.display()))?;

    let parsed = Wasm::parse(&wasm_bytes)
        .map_err_into_io()
        .with_message(|| format!("failed to parse {}", wasm.display()))?;

    // println!(
    //     "Found {} imports amounting to {} bytes",
    //     parsed.imports.len(),
    //     parsed.imports.iter().map(|b| b.name.len()).sum::<usize>()
    // );

    let js_content =
        fs::read_to_string(js).with_message(|| format!("failed to read {}", js.display()))?;

    let mut remaining = js_content.as_str();

    let mut sym = String::with_capacity(4);
    let mut js_new = String::with_capacity(js_content.len());
    let mut wasm_new = Vec::with_capacity(wasm_bytes.len());
    let mut wasm_imports = Vec::with_capacity(parsed.size);

    wasm_new.extend_from_slice(parsed.head);

    leb128_write(&mut wasm_imports, parsed.imports.len() as u64).expect("write to vec");

    let mut saved = 0;

    let Some(idx) = remaining.find(".wbg") else {
        panic!("Couldn't find the `wbg` imports in the JavaScript input");
    };

    js_new.push_str(&remaining[..idx]);
    js_new.push_str("._");

    remaining = &remaining[idx + 4..];

    for (n, import) in parsed.imports.into_iter().enumerate() {
        let Some(mut idx) = remaining.find(import.name) else {
            panic!(
                "Couldn't find the import {} in the JavaScript input",
                import.name
            );
        };

        if !remaining[..idx].ends_with(".wbg.") {
            continue;
        }

        idx -= 5;

        symbol(n, &mut sym);

        log::info!("renaming wbg.{} to _.{sym}", import.name);

        saved += (2 + import.name.len()) as isize - sym.len() as isize;

        js_new.push_str(&remaining[..idx]);
        js_new.push_str("._.");
        js_new.push_str(&sym);
        wasm_imports.extend_from_slice(b"\x01_");
        wasm_imports.push(sym.len() as u8);
        wasm_imports.extend_from_slice(sym.as_bytes());
        wasm_imports.extend_from_slice(import.ty);

        remaining = &remaining[idx + import.name.len() + 5..];
    }

    js_new.push_str(remaining);

    leb128_write(&mut wasm_new, wasm_imports.len() as u64).expect("write to vec");

    wasm_new.extend_from_slice(&wasm_imports);
    wasm_new.extend_from_slice(parsed.tail);

    log::info!("reduced both .wasm and .js files by {saved} bytes");

    fs::write(js, js_new).with_message(|| format!("failed to write {} file", js.display()))?;
    fs::write(wasm, wasm_new)
        .with_message(|| format!("failed to write {} file", wasm.display()))?;

    Ok(())
}

fn symbol(mut n: usize, buf: &mut String) {
    pub const ALPHABET: [u8; 52] = *b"abcdefghijklmnopqrstuvwxyz\
                                      ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                                      ";

    buf.clear();

    loop {
        let byte = ALPHABET[n % 52];

        buf.push(byte as char);

        n /= 52;

        if n == 0 {
            break;
        }

        n -= 1;
    }
}

struct Wasm<'source> {
    /// Byte size of the import section before transforms
    size: usize,
    /// Wasm blob before import section
    head: &'source [u8],
    /// All imports
    imports: Vec<Import<'source>>,
    /// Wasm blob following import section
    tail: &'source [u8],
}

impl Debug for Wasm<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Wasm")
            .field("head", &self.head)
            .field("imports", &self.imports)
            .field("tail", &self.tail.len())
            .finish()
    }
}

impl<'source> Wasm<'source> {
    fn parse(source: &'source [u8]) -> Result<Self, BinaryReaderError> {
        use wasmparser::{Parser, Payload};

        let parser = Parser::new(0);

        let mut out = Wasm {
            size: 0,
            head: &[],
            imports: Vec::new(),
            tail: &[],
        };

        for payload in parser.parse_all(source) {
            let section = match payload? {
                Payload::ImportSection(s) => s,
                Payload::ExportSection(s) => {
                    log::info!(
                        "found an export section {:?} {:?}",
                        s.range(),
                        &source[s.range()]
                    );

                    let mut iter = s.into_iter_with_offsets();

                    while let Some(Ok((index, export))) = iter.next() {
                        log::info!("export at {index}: {}", export.name);
                    }
                    continue;
                }
                _ => continue,
            };

            log::info!("found an import section");

            let mut head = &source[..section.range().start - 1];
            let size = section.range().end - section.range().start;
            let wasm = &source[section.range()];
            let tail = &source[section.range().end..];

            while let Some(last) = head.last() {
                // Stop if we are on the import section ID
                if *last == 2 {
                    break;
                }

                head = &head[..head.len() - 1];
            }

            let mut i = 0;
            let mut imports = Vec::with_capacity(section.count() as usize);

            while i < wasm.len() && !wasm[i..].starts_with(b"\x03wbg") {
                i += 1;
            }

            while i < wasm.len() && wasm[i..].starts_with(b"\x03wbg") {
                let len = wasm[i + 4] as usize;

                i += 5;

                let slice = wasm
                    .get(i..i + len)
                    .expect("Couldn't read import name from Wasm");

                let name = std::str::from_utf8(slice).expect("Invalid import name");

                i += len;

                imports.push(Import {
                    name,
                    ty: &wasm[i..i + 2],
                });

                i += 2;
                continue;
            }

            if imports.len() != section.count() as usize {
                panic!("Some imports not found?");
            }

            out.size = size;
            out.head = head;
            out.imports = imports;
            out.tail = tail;
        }

        Ok(out)
    }
}

struct Import<'source> {
    // Found name
    name: &'source str,
    // Unprased Wasm bytes for the type of this import
    ty: &'source [u8],
}

impl Debug for Import<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {:?}", self.name, self.ty)
    }
}

fn read_file_paths(path: &Path) -> io::Result<Vec<PathBuf>> {
    let mut paths = vec![];
    let mut to_visit = vec![Cow::Borrowed(path)];
    while let Some(dir) = to_visit.pop() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            if file_type.is_file() {
                paths.push(entry.path());
            } else if file_type.is_dir() {
                to_visit.push(Cow::Owned(entry.path()));
            }
        }
    }

    Ok(paths)
}

struct Paths<'path> {
    dist: Dist<'path>,
    snippets: &'path [PathBuf],
    wasm: &'path Path,
    js: &'path Path,
    index: &'path Path,
}

#[derive(Clone, Copy)]
struct Dist<'path>(&'path Path);

impl Dist<'_> {
    fn embed_path(self, path: &Path) -> impl Display + use<'_> {
        struct Show<'path>(&'path Path);

        impl Display for Show<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "/{}", self.0.display())
            }
        }

        Show(
            path.strip_prefix(self.0)
                .expect("the path must have dist prefix"),
        )
    }
}

struct MakeIndex<'path> {
    orig_index: &'path Path,
    paths: Paths<'path>,
    embed_autoreload_script: bool,
}

fn make_index_html(m: MakeIndex) -> Report<()> {
    let MakeIndex {
        orig_index,
        paths,
        embed_autoreload_script,
    } = m;

    let js_link = |p| {
        format!(
            r#"<link rel="modulepreload" href="{}" crossorigin=anonymous>"#,
            paths.dist.embed_path(p),
        )
    };

    let wasm_link = |p| {
        format!(
            r#"<link rel="preload" href="{}" crossorigin=anonymous as="fetch" type="application/wasm">"#,
            paths.dist.embed_path(p),
        )
    };

    let js_script = format!(
        "<script type=\"module\">\n\
            import init, * as bindings from '{}';\n\
            window.wasmBindings = bindings;\n\
            await init({{ module_or_path: '{}' }});\n\
        </script>\n",
        paths.dist.embed_path(paths.js),
        paths.dist.embed_path(paths.wasm),
    );

    let html = fs::read_to_string(orig_index)
        .or_else(|err| {
            if err.kind() == io::ErrorKind::NotFound {
                Ok(include_str!("../init/index.html").to_owned())
            } else {
                Err(err)
            }
        })
        .with_message(|| format!("failed to read {}", orig_index.display()))?;

    let mut embed_links = Some(|el: &mut Element| {
        el.append(&js_link(paths.js), ContentType::Html);
        el.append(&wasm_link(paths.wasm), ContentType::Html);
        for snippet in paths.snippets {
            if let Some(ext) = snippet.extension() {
                if ext == "js" {
                    el.append(&js_link(snippet), ContentType::Html);
                }
            }
        }
    });

    let mut embed_js_script = Some(|el: &mut Element| {
        el.append(&js_script, ContentType::Html);

        if embed_autoreload_script {
            el.append("<script>", ContentType::Html);
            el.append(include_str!("../reload.js"), ContentType::Html);
            el.append("</script>", ContentType::Html);
        }
    });

    let settings = RewriteStrSettings {
        element_content_handlers: vec![
            element!("head", |el| {
                if let Some(f) = embed_links.take() {
                    f(el);
                }

                Ok(())
            }),
            element!("body", |el| {
                if let Some(f) = embed_js_script.take() {
                    f(el);
                }

                Ok(())
            }),
        ],
        ..RewriteStrSettings::new()
    };

    let html_new = rewrite_str(&html, settings)
        .map_err_into_io()
        .message("failed to rewrite html")?;

    if embed_links.is_some() {
        return Err(Error::message(format!(
            "<head> tag not found in {} file",
            orig_index.display(),
        )));
    }

    if embed_js_script.is_some() {
        return Err(Error::message(format!(
            "<body> tag not found in {} file",
            orig_index.display(),
        )));
    }

    fs::write(paths.index, html_new)
        .with_message(|| format!("failed to write {} file", paths.index.display()))?;

    Ok(())
}
