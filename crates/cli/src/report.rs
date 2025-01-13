use std::error;
use std::fmt;
use std::io;

pub type Report<T> = Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    err: Option<io::Error>,
    message: String,
}

impl Error {
    pub fn new<U, M>(err: U, message: M) -> Self
    where
        U: Into<io::Error>,
        M: Into<String>,
    {
        Self {
            err: Some(err.into()),
            message: message.into(),
        }
    }

    pub fn message<M>(message: M) -> Self
    where
        M: Into<String>,
    {
        Self {
            err: None,
            message: message.into(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(err) = &self.err {
            write!(f, ": {err}")?;
        }

        Ok(())
    }
}

pub trait ErrorExt<T, E> {
    fn map_err_into_io(self) -> Result<T, io::Error>
    where
        E: Into<Box<dyn error::Error + Send + Sync>>;

    fn message(self, message: &str) -> Result<T, Error>
    where
        E: Into<io::Error>;

    fn with_message<F, M>(self, f: F) -> Result<T, Error>
    where
        E: Into<io::Error>,
        F: FnOnce() -> M,
        M: Into<String>;
}

impl<T, E> ErrorExt<T, E> for Result<T, E> {
    fn map_err_into_io(self) -> Result<T, io::Error>
    where
        E: Into<Box<dyn error::Error + Send + Sync>>,
    {
        self.map_err(io::Error::other)
    }

    fn message(self, message: &str) -> Result<T, Error>
    where
        E: Into<io::Error>,
    {
        self.map_err(|err| Error::new(err, message))
    }

    fn with_message<F, M>(self, f: F) -> Result<T, Error>
    where
        E: Into<io::Error>,
        F: FnOnce() -> M,
        M: Into<String>,
    {
        self.map_err(|err| Error::new(err, f()))
    }
}
