use anyhow::{bail, Context};
use encoding_rs::Encoding;
use log::*;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    fmt::Display,
    fs::{File, OpenOptions},
    io::{self, IsTerminal, Read, Seek, SeekFrom, Write},
    path::{Path, PathBuf},
    time::Instant,
};

use glob::glob;
use pasfmt_core::formatter::Formatter;
use rayon::prelude::*;
use walkdir::WalkDir;

#[derive(Deserialize, Serialize)]
pub struct Cache {
    hash_by_path: FxHashMap<String, u64>,
}

use crate::ErrHandler;

type WriteResult = std::io::Result<u64>;

struct DecodedFile<'a> {
    bom: Option<&'a [u8]>,
    contents: Cow<'a, str>,
    encoding: &'static Encoding,
}

pub struct FileFormatter {
    formatter: Formatter,
    encoding: &'static encoding_rs::Encoding,
}
impl FileFormatter {
    pub fn new(formatter: Formatter, encoding: &'static encoding_rs::Encoding) -> Self {
        FileFormatter {
            formatter,
            encoding,
        }
    }

    fn expand_paths<S: AsRef<str>>(&self, paths: &[S]) -> Vec<Result<PathBuf, anyhow::Error>> {
        let mut expanded_paths = vec![];
        paths.iter().map(AsRef::as_ref).for_each(|path_str| {
            let path = Path::new(path_str);
            match path.metadata() {
                Ok(metadata) if metadata.is_dir() => {
                    expanded_paths.extend(WalkDir::new(path_str).into_iter().filter_map(|entry| {
                        match entry {
                            Ok(entry) => {
                                let file_path = entry.path();
                                match formattable_file_path(file_path) {
                                    true => Some(Ok(file_path.to_path_buf())),
                                    false => None,
                                }
                            }
                            Err(e) => Some(Err(e.into())),
                        }
                    }));
                }
                _ if path_str.contains('*') => {
                    match glob(path_str)
                        .with_context(|| format!("invalid glob expression `{path_str}`"))
                    {
                        Err(e) => expanded_paths.push(Err(e)),
                        Ok(glob) => {
                            expanded_paths.extend(glob.map(|entry| entry.map_err(|e| e.into())))
                        }
                    }
                }
                _ => {
                    expanded_paths.push(Ok(path.to_path_buf()));
                }
            }
        });

        expanded_paths
    }

    fn decode_file<'a>(
        &self,
        mut file: impl Read,
        name: impl Display,
        buf: &'a mut Vec<u8>,
    ) -> anyhow::Result<DecodedFile<'a>> {
        file.read_to_end(buf)?;

        let (encoding, (bom, contents)) = match Encoding::for_bom(buf) {
            Some((encoding, bom_length)) => {
                (encoding, (Some(&buf[..bom_length]), &buf[bom_length..]))
            }
            None => (self.encoding, (None, &buf[..])),
        };
        let (contents, replacements) = encoding.decode_without_bom_handling(contents);

        if replacements {
            bail!(
                "File '{}' has malformed sequences (in encoding '{}'{})",
                name,
                encoding.name(),
                bom.map(|_| " - inferred from BOM").unwrap_or(""),
            );
        }

        Ok(DecodedFile {
            bom,
            contents,
            encoding,
        })
    }

    fn exec_format<S, T, E>(
        &self,
        paths: &[S],
        mut open_options: OpenOptions,
        result_operation: T,
        error_handler: E,
    ) where
        S: AsRef<str>,
        E: ErrHandler,
        T: Fn(&mut File, &Path, &DecodedFile, &str) -> anyhow::Result<()> + Sync,
    {
        let mut cache = get_cache();

        open_options.read(true);

        let cache_results: Vec<Option<(PathBuf, u64)>> = self
            .expand_paths(paths)
            .into_par_iter()
            .map_init(
                || (Vec::<u8>::new(), String::new()),
                |(input_buf, output_buf), file_path| -> anyhow::Result<Option<(PathBuf, u64)>> {
                    input_buf.clear();
                    output_buf.clear();

                    let file_path = file_path?;
                    let mut file = open_options
                        .open(&file_path)
                        .with_context(|| format!("failed to open '{}'", file_path.display()))?;

                    let decoded_file = self
                        .decode_file(&mut file, file_path.display(), input_buf)
                        .with_context(|| format!("failed to read '{}'", file_path.display()))?;

                    let new_hash = seahash::hash(decoded_file.contents.as_bytes());
                    if let Some(old_hash) = cache
                        .hash_by_path
                        .get(file_path.as_os_str().to_str().unwrap())
                    {
                        if *old_hash == new_hash {
                            log::debug!("cache hit on {}", file_path.display());
                            return Ok(None);
                        }
                        log::debug!("cache miss on {}", file_path.display());
                    }

                    debug!("Formatting {}", file_path.display());
                    let time = Instant::now();
                    self.formatter
                        .format_into_buf(&decoded_file.contents, output_buf);
                    debug!("Formatted {} in {:?}", file_path.display(), time.elapsed());
                    result_operation(&mut file, &file_path, &decoded_file, output_buf)
                        .map(|_| Some((file_path, new_hash)))
                },
            )
            .flat_map(|res| match res {
                Err(e) => {
                    error_handler(e);
                    None
                }
                Ok(x) => Some(x),
            })
            .collect();

        cache_results
            .into_iter()
            .flatten()
            .for_each(|(file_path, new_hash)| {
                cache
                    .hash_by_path
                    .insert(file_path.to_string_lossy().to_string(), new_hash);
            });

        set_cache(cache);
    }

    pub(crate) fn format_files<S: AsRef<str>>(&self, paths: &[S], error_handler: impl ErrHandler) {
        self.exec_format(
            paths,
            OpenOptions::new().write(true).to_owned(),
            |file, file_path, decoded_file, formatted_output| {
                if decoded_file.contents.eq(&formatted_output) {
                    debug!(
                        "skipping writing to '{}' because it is already formatted",
                        file_path.display()
                    );
                    return Ok(());
                }
                file.seek(SeekFrom::Start(0)).with_context(|| {
                    format!("failed to seek to start of file: '{}'", file_path.display())
                })?;
                let new_len = Self::write_file(&mut *file, decoded_file, formatted_output)
                    .with_context(|| format!("failed to write to '{}'", file_path.display()))?;
                file.set_len(new_len).with_context(|| {
                    format!("failed to set file length: '{}'", file_path.display())
                })?;
                Ok(())
            },
            error_handler,
        );
    }

    fn encode_utf16(data: &str, u16_encoder: impl Fn(u16) -> [u8; 2]) -> Vec<u8> {
        // 2 * utf8_len is a pretty good approximation for utf16_len in most cases.
        let mut out = Vec::with_capacity(2 * data.len());
        out.extend(data.encode_utf16().flat_map(u16_encoder));
        out
    }

    fn encode_utf16be(data: &str) -> Vec<u8> {
        Self::encode_utf16(data, u16::to_be_bytes)
    }
    fn encode_utf16le(data: &str) -> Vec<u8> {
        Self::encode_utf16(data, u16::to_le_bytes)
    }

    fn encode<'a>(encoding: &'static Encoding, data: &'a str) -> io::Result<Cow<'a, [u8]>> {
        // encoding_rs doesn't support encoding to UTF16, so we do it ourselves.
        if encoding.output_encoding() == encoding {
            let (encoded_data, _encoding_used, replacements) = encoding.encode(data);
            if replacements {
                Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "Formatting result contains data that cannot be encoded as {}",
                        encoding.name()
                    ),
                ))
            } else {
                Ok(encoded_data)
            }
        } else if encoding == encoding_rs::UTF_16BE {
            Ok(Cow::Owned(Self::encode_utf16be(data)))
        } else if encoding == encoding_rs::UTF_16LE {
            Ok(Cow::Owned(Self::encode_utf16le(data)))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                format!("No encoder for encoding {}", encoding.name()),
            ))
        }
    }

    fn write(
        mut write: impl Write,
        encoding: &'static Encoding,
        bom: Option<&[u8]>,
        data: &str,
    ) -> WriteResult {
        let encoded_output = Self::encode(encoding, data)?;

        let mut len = 0;
        if let Some(bom) = bom {
            write.write_all(bom)?;
            len += bom.len();
        }
        write.write_all(&encoded_output)?;
        len += encoded_output.len();

        Ok(len as u64)
    }

    fn write_file(write: impl Write, decoded_file: &DecodedFile, data: &str) -> WriteResult {
        Self::write(write, decoded_file.encoding, decoded_file.bom, data)
    }

    fn write_stdout(decoded_file: &DecodedFile, data: &str) -> WriteResult {
        let stdout = std::io::stdout().lock();
        let (encoding, bom) = if stdout.is_terminal() {
            /*
               stdout is a terminal, so we use UTF-8 as the output encoding because when std::io
               writes to a Windows console it uses the W variants of the winapi functions which
               require code units as u16. std::io takes the approach of converting the input from
               UTF-8 to UTF-16, and returning an error if the input wasn't valid UTF-8.

               See https://github.com/rust-lang/rust/issues/23344#issuecomment-228208528
            */
            debug!("writing output as UTF-8 because stdout was detected to be a console");
            (encoding_rs::UTF_8, None)
        } else {
            (decoded_file.encoding, decoded_file.bom)
        };

        Self::write(stdout, encoding, bom, data)
    }

    fn decode_stdin<'a>(&self, buf: &'a mut Vec<u8>) -> anyhow::Result<DecodedFile<'a>> {
        let stdin = std::io::stdin().lock();
        if stdin.is_terminal() {
            eprintln!("waiting for stdin...");
        }
        self.decode_file(stdin, "<stdin>", buf)
            .context("failed to read from stdin")
    }

    pub(crate) fn format_stdin_to_stdout(&self, error_handler: impl ErrHandler) {
        let inner = || {
            let mut buf = vec![];
            let decoded_stdin = self.decode_stdin(&mut buf)?;
            let formatted_input = self.formatter.format(&decoded_stdin.contents);
            Self::write_stdout(&decoded_stdin, &formatted_input)
                .context("failed to write to stdout")?;
            Ok(())
        };

        if let Err(e) = inner() {
            error_handler(e);
        }
    }

    pub(crate) fn format_files_to_stdout<S: AsRef<str>>(
        &self,
        paths: &[S],
        error_handler: impl ErrHandler,
    ) {
        self.exec_format(
            paths,
            OpenOptions::new(),
            |_, file_path, _, formatted_output| {
                /*
                   Write the output as UTF-8 — don't re-encode it.

                   In this case, all the files are being concatenated to stdout.
                   Each file could have a different encoding, so it doesn't make sense use the
                   original encodings.

                   Also, append an additional newline because makes it more human-readable and no
                   less machine-readable.
                */
                print!("{}:\n{}\n", file_path.display(), formatted_output);
                Ok(())
            },
            error_handler,
        );
    }

    fn check_formatting(input: &str, output: &str, path: impl Display) -> anyhow::Result<()> {
        if input != output {
            bail!("CHECK: '{}' has incorrect formatting", path);
        }
        Ok(())
    }

    pub(crate) fn check_files<S: AsRef<str>>(&self, paths: &[S], error_handler: impl ErrHandler) {
        self.exec_format(
            paths,
            OpenOptions::new(),
            |_, file_path, decoded_file, formatted_output| {
                Self::check_formatting(
                    &decoded_file.contents,
                    formatted_output,
                    file_path.display(),
                )
            },
            error_handler,
        );
    }

    pub(crate) fn check_stdin(&self, error_handler: impl ErrHandler) {
        let inner = || {
            let mut buf = vec![];
            let decoded_stdin = self.decode_stdin(&mut buf)?;
            let formatted_input = self.formatter.format(&decoded_stdin.contents);
            Self::check_formatting(&decoded_stdin.contents, &formatted_input, "<stdin>")
        };

        if let Err(e) = inner() {
            error_handler(e);
        }
    }
}

fn get_cache() -> Cache {
    std::fs::read_to_string("C:/Users/zad/Documents/Code/pasfmt/cache.json")
        .map(|s| serde_json::from_str(&s).unwrap())
        .unwrap_or_else(|_| Cache {
            hash_by_path: FxHashMap::default(),
        })
}

fn set_cache(cache: Cache) {
    std::fs::write(
        "C:/Users/zad/Documents/Code/pasfmt/cache.json",
        serde_json::to_string(&cache).unwrap(),
    )
    .unwrap();
}

fn formattable_file_path(path: &Path) -> bool {
    match path.extension() {
        Some(ext) => {
            ext.eq_ignore_ascii_case("pas")
                || ext.eq_ignore_ascii_case("dpr")
                || ext.eq_ignore_ascii_case("dpk")
        }
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::file_formatter::formattable_file_path;
    use std::path::PathBuf;

    #[parameterized(
        pas_lower = {"a.pas"},
        pas_upper = {"a.PAS"},
        pas_mixed = {"a.Pas"},
        dpr_lower = {"b.dpr"},
        dpr_upper = {"b.DPR"},
        dpr_mixed = {"b.Dpr"},
        dpk_lower = {"c.dpk"},
        dpk_upper = {"c.DPK"},
        dpk_mixed = {"c.Dpk"},
    )]
    fn formattable_file_paths(path: &str) {
        assert!(formattable_file_path(&PathBuf::from(path)));
    }

    #[parameterized(
        no_dot_pas = {"pas"},
        starts_pas = {"a.pas1"},
        ends_pas = {"a.unpas"},
        contains_dot_pas = {"a.pas.x"},
        no_dot_dpr = {"dpr"},
        starts_dpr = {"a.dpr1"},
        ends_dpr = {"a.undpr"},
        contains_dot_dpr = {"a.dpr.x"},
        no_dot_dpk = {"dpk"},
        starts_dpk = {"a.dpk1"},
        ends_dpk = {"a.undpk"},
        contains_dot_dpk = {"a.dpk.x"},
        only_dot_pas = {".pas"},
        only_dot_dpr = {".dpr"},
        only_dot_dpk = {".dpk"},
    )]
    fn non_formattable_file_paths(path: &str) {
        assert!(!formattable_file_path(&PathBuf::from(path)));
    }
}
