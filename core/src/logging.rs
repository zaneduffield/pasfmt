use core::fmt;
use std::{error::Error, fmt::Display, sync::RwLock};

use log::{log, Level};

use scoped_tls_hkt::scoped_thread_local;

#[derive(Copy, Clone)]
pub struct LogContext<'a> {
    pub source: &'a str,
    pub data_to_fmt: &'a str,
}

pub enum FilePos {
    Raw { pos: usize, len: usize },
    Token(usize),
    LineCol(usize, usize),
}

pub struct LogEvent<'a> {
    pub args: fmt::Arguments<'a>,
    pub level: Level,
    pub file_pos: Option<FilePos>,
}

pub type PasfmtLogFn = fn(Option<LogContext>, LogEvent);

#[derive(Copy, Clone)]
pub struct PasfmtLogger<'a> {
    pub fun: PasfmtLogFn,
    pub ctx: Option<LogContext<'a>>,
}

fn default_log_fn(_ctx: Option<LogContext>, event: LogEvent) {
    log!(event.level, "{}", event.args);
}

static PASFMT_LOG_FN: RwLock<PasfmtLogFn> = RwLock::new(default_log_fn);

scoped_thread_local! {
  pub static PASFMT_LOGGER: for <'a> PasfmtLogger<'a>
}

#[derive(Debug)]
pub enum LockErr {
    Read,
    Write,
}
impl Display for LockErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LockErr::Read => {
                f.write_str("Failed to lock the static logger function with read access")
            }
            LockErr::Write => {
                f.write_str("Failed to lock the static logger function with write access")
            }
        }
    }
}

impl Error for LockErr {}

pub fn with_log_fn<T>(fun: PasfmtLogFn, mut f: impl FnMut() -> T) -> Result<T, LockErr> {
    let prev = *PASFMT_LOG_FN.try_read().map_err(|_| LockErr::Read)?;

    *PASFMT_LOG_FN.try_write().map_err(|_| LockErr::Write)? = fun;

    let out = f();

    *PASFMT_LOG_FN.try_write().map_err(|_| LockErr::Write)? = prev;

    Ok(out)
}

pub fn with_log_context<T>(ctx: LogContext<'_>, f: impl FnMut() -> T) -> T {
    PASFMT_LOGGER.set(
        PasfmtLogger {
            fun: *PASFMT_LOG_FN.read().expect("failed to obtain log fn lock"),
            ctx: Some(ctx),
        },
        f,
    )
}

#[macro_export]
macro_rules! pasfmt_log {
    ($level: expr, pos = $pos: expr, $($arg: expr),*$(,)?) => {
        // if log_enabled!($level) {
            $crate::logging::pasfmt_log($level, Some($pos), format_args!($($arg),*));
        // }
    };
    ($level: expr, $($arg: expr),*$(,)?) => {
        // if log_enabled!($level) {
            $crate::logging::pasfmt_log($level, None, format_args!($($arg),*));
        // }
    };
}

pub fn pasfmt_log(level: Level, pos: Option<FilePos>, args: fmt::Arguments<'_>) {
    if PASFMT_LOGGER.is_set() {
        PASFMT_LOGGER.with(|log| {
            (log.fun)(
                log.ctx,
                LogEvent {
                    args,
                    level,
                    file_pos: pos,
                },
            )
        })
    } else {
        log!(level, "{}", args);
    }
}
