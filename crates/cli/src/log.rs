use std::fmt;
use std::sync::OnceLock;

use crossterm::style::Stylize;

static VERBOSE_OUTPUT: OnceLock<()> = OnceLock::new();

pub fn enable_verbose_output() {
    let _ = VERBOSE_OUTPUT.set(());
}

pub fn is_verbose_output_enabled() -> bool {
    VERBOSE_OUTPUT.get().is_some()
}

static COLOR_OUTPUT: OnceLock<()> = OnceLock::new();

pub fn enable_color_output() {
    let _ = COLOR_OUTPUT.set(());
}

pub fn is_color_output_enabled() -> bool {
    COLOR_OUTPUT.get().is_some()
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {{
        eprintln!("{}: {}", $crate::log::Error, format_args!($($arg)*));
    }};
}

pub use error;

#[macro_export]
macro_rules! building {
    ($($arg:tt)*) => {{
        eprintln!("{} {}", $crate::log::Title("    Building"), format_args!($($arg)*));
    }};
}

pub use building;

#[macro_export]
macro_rules! optimized {
    ($($arg:tt)*) => {{
        eprintln!("{} {}", $crate::log::Title("   Optimized"), format_args!($($arg)*));
    }};
}

pub use optimized;

#[macro_export]
macro_rules! creating {
    ($($arg:tt)*) => {{
        eprintln!("{} {}", $crate::log::Title("    Creating"), format_args!($($arg)*));
    }};
}

pub use creating;

#[macro_export]
macro_rules! starting {
    ($($arg:tt)*) => {{
        eprintln!("{} {}", $crate::log::Title("    Starting"), format_args!($($arg)*));
    }};
}

pub use starting;

#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {{
        if $crate::log::is_verbose_output_enabled() {
            eprintln!("{} {}", $crate::log::Title("        Info"), format_args!($($arg)*));
        }
    }};
}

pub use info;

pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let title = "error";
        if is_color_output_enabled() {
            write!(f, "{}", title.dark_red().bold())
        } else {
            write!(f, "{title}")
        }
    }
}

pub struct Title(pub &'static str);

impl fmt::Display for Title {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let title = self.0;
        if is_color_output_enabled() {
            write!(f, "{}", title.dark_blue().bold())
        } else {
            write!(f, "{title}")
        }
    }
}
