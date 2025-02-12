use std::process::ExitCode;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use log::error;
use pasfmt::{format, FormattingConfig};
use pasfmt_core::prelude::*;
use pasfmt_orchestrator::predule::*;

pasfmt_config!(
    #[command(bin_name = "pasfmt")]
    Config<FormattingConfig>
);

fn main() -> ExitCode {
    with_log_fn(
        |ctx: Option<LogContext<'_>>, event: LogEvent| {
            if let Some(_ctx) = ctx {
                log::log!(
                    event.level,
                    "{}",
                    event.msg // "path: {}\n\ncontents: {}\n\nmsg: {}",
                              // ctx.path.display(),
                              // ctx.data_to_fmt,
                              // event.msg
                );
            } else {
                log::log!(event.level, "{}", event.msg);
            }
        },
        || {
            let config = Config::create();
            stderrlog::new()
                .verbosity(config.log_level())
                .init()
                .unwrap();

            let had_error = AtomicBool::new(false);

            let err_handler = |e| {
                had_error.store(true, Ordering::Relaxed);
                error!("{:?}", e);
            };

            format(config, err_handler);

            if had_error.into_inner() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        },
    )
    .unwrap()
}
