use std::{error::Error, fmt::Display, path::Path, sync::RwLock};

use log::{log, Level};

use scoped_tls_hkt::scoped_thread_local;

#[derive(Copy, Clone)]
pub struct LogContext<'a> {
    pub path: &'a Path,
    pub data_to_fmt: &'a str,
}

pub enum FilePos {
    Raw(usize),
    Token(usize),
    LineCol(usize, usize),
}

pub struct LogEvent<'a> {
    pub msg: &'a str,
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
    log!(event.level, "{}", event.msg);
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
    ($level: expr, $($arg: expr),*$(,)?) => {
        if log_enabled!($level) {
            $crate::logging::pasfmt_log($level, &format!($($arg),*));
        }
    };
}

pub fn pasfmt_log(level: Level, msg: &str) {
    if PASFMT_LOGGER.is_set() {
        PASFMT_LOGGER.with(|log| {
            (log.fun)(
                log.ctx,
                LogEvent {
                    msg,
                    level,
                    file_pos: None,
                },
            )
        })
    } else {
        log!(level, "{}", msg);
    }
}
