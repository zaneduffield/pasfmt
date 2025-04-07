#![forbid(unsafe_code)]
#![deny(clippy::enum_glob_use)]
#![cfg_attr(coverage_nightly, feature(coverage_attribute))]

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
