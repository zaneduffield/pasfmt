use std::path::Path;

use log::{log, log_enabled, Level};

use scoped_tls_hkt::scoped_thread_local;

scoped_thread_local! {
  pub static LOG_CONTEXT: for <'a> LogContext<'a>
}

#[derive(Copy, Clone)]
pub struct LogContext<'a> {
    pub path: &'a Path,
    pub file_contents: &'a str,
}

pub fn pasfmt_log(msg: &str, level: Level) {
    if log_enabled!(level) {
        if LOG_CONTEXT.is_set() {
            LOG_CONTEXT.with(|context| {
                log!(
                    level,
                    "path: {}\n\ncontents: {}\n\nmsg: {}",
                    context.path.display(),
                    context.file_contents,
                    msg
                );
            })
        } else {
            log!(level, "{msg}");
        }
    }
}
