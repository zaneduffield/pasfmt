use std::process::ExitCode;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::StandardStream;
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
            if let Some(ctx) = ctx {
                if let Some(file_pos) = event.file_pos {
                    let mut files = SimpleFiles::new();
                    let file_id = files.add(ctx.source, ctx.data_to_fmt);
                    let severity = match event.level {
                        log::Level::Error => Severity::Error,
                        log::Level::Warn => Severity::Warning,
                        _ => Severity::Note,
                    };

                    let diag = Diagnostic::new(severity)
                        .with_message(format!("{}", event.args))
                        .with_labels(vec![Label::primary(
                            file_id,
                            match file_pos {
                                FilePos::Raw { pos, len } => pos..(pos + len),
                                _ => panic!(),
                            },
                        )]);

                    let writer = StandardStream::stderr(termcolor::ColorChoice::Always);
                    let config = term::Config::default();
                    term::emit(&mut writer.lock(), &config, &files, &diag).expect("failed to log");
                    return;
                }
                // log!(
                //     event.level,
                //     "path: {}\n\ncontents: {}\n\nmsg: {}",
                //     ctx.path.display(),
                //     ctx.data_to_fmt,
                //     event.args,
                // );
            }
            log::log!(event.level, "{}", event.args);
        },
        || {
            let config = Config::create();

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
