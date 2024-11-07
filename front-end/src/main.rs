use export_all::format_with_settings;
use export_all::FormattingSettings;
use pasfmt_orchestrator::predule::*;

pasfmt_config!(
    #[command(bin_name = "export-all")]
    Config
);

fn main() -> anyhow::Result<()> {
    let config = Config::create();
    stderrlog::new()
        .verbosity(config.log_level())
        .init()
        .unwrap();

    let formatting_settings = config.get_config_object::<FormattingSettings>()?;
    format_with_settings(formatting_settings, config)
}
