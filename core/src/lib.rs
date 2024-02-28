#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::needless_pass_by_value,
    clippy::needless_for_each,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc
)]

pub mod defaults;
pub mod formatter;
pub mod formatter_selector;
pub mod lang;
pub mod rules;
pub mod test_utils;
pub mod traits;

pub mod prelude {
    pub use crate::defaults::*;
    pub use crate::formatter::*;
    pub use crate::formatter_selector::*;
    pub use crate::lang::*;
    pub use crate::rules::*;
    pub use crate::traits::*;

    #[cfg(test)]
    pub use crate::test_utils::*;
}
