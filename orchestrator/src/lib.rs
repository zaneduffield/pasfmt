#![forbid(unsafe_code)]

pub mod command_line;
pub mod file_formatter;
pub mod formatting_orchestrator;

pub trait ErrHandler: Fn(anyhow::Error) + Sync {}
impl<T: Fn(anyhow::Error) + Sync> ErrHandler for T {}

pub mod prelude {
    pub use super::*;
    pub use crate::command_line::{pasfmt_config, *};
    pub use crate::file_formatter::*;
    pub use crate::formatting_orchestrator::*;
}
