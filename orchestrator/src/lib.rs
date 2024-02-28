#![forbid(unsafe_code)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::needless_pass_by_value,
    clippy::needless_for_each,
    clippy::missing_errors_doc
)]

pub mod command_line;
pub mod file_formatter;
pub mod formatting_orchestrator;

pub trait ErrHandler: Fn(anyhow::Error) + Sync {}
impl<T: Fn(anyhow::Error) + Sync> ErrHandler for T {}

pub mod predule {
    pub use super::*;
    pub use crate::command_line::{pasfmt_config, *};
    pub use crate::file_formatter::*;
    pub use crate::formatting_orchestrator::*;
}
