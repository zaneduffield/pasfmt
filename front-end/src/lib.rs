#![deny(clippy::enum_glob_use)]

use encoding_rs::Encoding;
use pasfmt_core::prelude::*;
use pasfmt_orchestrator::predule::*;
use serde_derive::Deserialize;

#[cfg(windows)]
fn windows_default_encoding() -> &'static Encoding {
    use log::warn;

    // SAFETY: yes it's a foreign function, but it's a simple one from the WinAPI that we
    // can assume to be safe.
    let oemcp = unsafe { windows::Win32::Globalization::GetACP() };
    oemcp
        .try_into()
        .ok()
        .and_then(|cp: u16| codepage::to_encoding(cp))
        .unwrap_or_else(|| {
            warn!("Failed to convert system codepage to encoding. Defaulting to UTF-8.");
            encoding_rs::UTF_8
        })
}

fn default_encoding() -> &'static Encoding {
    #[cfg(windows)]
    {
        windows_default_encoding()
    }
    #[cfg(not(windows))]
    encoding_rs::UTF_8
}

#[derive(Deserialize, Debug)]
pub struct FormattingSettings {
    reconstruction: Reconstruction,
    encoding: &'static Encoding,
}

impl Default for FormattingSettings {
    fn default() -> Self {
        Self {
            reconstruction: Default::default(),
            encoding: default_encoding(),
        }
    }
}

impl From<Reconstruction> for ReconstructionSettings {
    fn from(val: Reconstruction) -> Self {
        ReconstructionSettings::new(
            val.eol,
            val.indentation.clone(),
            val.continuation.unwrap_or(val.indentation),
        )
    }
}

#[derive(Deserialize, Debug)]
struct Reconstruction {
    eol: String,
    indentation: String,
    continuation: Option<String>,
}
impl Default for Reconstruction {
    fn default() -> Self {
        Reconstruction {
            eol: "\r\n".to_owned(),
            indentation: "  ".to_owned(),
            continuation: Some("  ".to_owned()),
        }
    }
}

pub fn format_with_settings(formatting_settings: FormattingSettings, config: PasFmtConfiguration) {
    let uses_clause_formatter = &UsesClauseFormatter {};
    let eof_newline_formatter = &EofNewline {};
    let formatter = FileFormatter::new(
        Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .lines_consolidator(PropertyDeclarationConsolidator {})
            .token_consolidator(DistinguishGenericTypeParamsConsolidator {})
            .lines_consolidator(ImportClauseConsolidator {})
            .token_ignorer(FormattingToggler {})
            .token_ignorer(IgnoreNonUnitImportClauses {})
            .file_formatter(TokenSpacing {})
            .line_formatter(RemoveRepeatedNewlines {})
            .line_formatter(FormatterSelector::new(
                |logical_line_type| match logical_line_type {
                    LogicalLineType::ImportClause => Some(uses_clause_formatter),
                    LogicalLineType::Eof => Some(eof_newline_formatter),
                    _ => None,
                },
            ))
            .reconstructor(DelphiLogicalLinesReconstructor::new(
                formatting_settings.reconstruction.into(),
            ))
            .build(),
        formatting_settings.encoding,
    );
    FormattingOrchestrator::run(formatter, config);
}
