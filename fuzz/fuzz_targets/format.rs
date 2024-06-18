#![no_main]

use std::hint::black_box;

use libfuzzer_sys::fuzz_target;
use pasfmt_core::prelude::*;

fuzz_target!(|input: String| {
    let recon_settings = ReconstructionSettings::new("\n", "  ", "    ").unwrap();

    let formatter = Formatter::builder()
        .lexer(DelphiLexer {})
        .parser(DelphiLogicalLineParser {})
        .token_consolidator(DistinguishGenericTypeParamsConsolidator {})
        .lines_consolidator(ImportClauseConsolidator {})
        .token_ignorer(FormattingToggler {})
        .token_ignorer(IgnoreNonUnitImportClauses {})
        .token_ignorer(IgnoreAsmIstructions {})
        .file_formatter(TokenSpacing {})
        .file_formatter(OptimisingLineFormatter::new(
            OptimisingLineFormatterSettings {
                max_line_length: 120,
                iteration_max: 20_000,
            },
            recon_settings.clone(),
        ))
        .reconstructor(DelphiLogicalLinesReconstructor::new(recon_settings))
        .build();
    black_box(formatter.format(&input));
});
