use anyhow::{Context, Result};
use clap::Parser;
use console::{style, Style};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

mod create_scripts;
mod diff;
mod ffi;
mod types;
mod util;

use types::*;
use util::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Settings {
    pub rootdir: PathBuf,
    pub dbpath: PathBuf,
    pub cachedir: Vec<String>,
    pub exts: Vec<String>,
    pub ignore: Vec<String>,
    pub ignorefs: Vec<String>,
    pub max_text_size: u32,
    pub min_merge_file: u32,
    #[serde(flatten)]
    pub flags: Flags,
}

impl Default for Settings {
    fn default() -> Self {
        toml::from_str(include_str!("default.toml")).unwrap()
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, clap::Args)]
pub struct Flags {
    #[serde(default, skip_serializing_if = "default")]
    #[clap(long)]
    pub ignore_permission_denied: bool,

    #[serde(default, skip_serializing_if = "default")]
    #[clap(long)]
    pub check_mtime: bool,

    #[serde(default, skip_serializing_if = "default")]
    #[clap(long)]
    pub show_text_diff: bool,

    #[serde(default, skip_serializing_if = "default")]
    #[clap(long)]
    pub skip_unmanaged_stats: bool,

    #[serde(default, skip_serializing_if = "default")]
    #[clap(long)]
    pub show_ignored: bool,

    #[serde(default, skip_serializing_if = "default")]
    #[clap(long)]
    pub outdir: Option<PathBuf>,
}

fn default<T: Default + PartialEq>(t: &T) -> bool {
    *t == Default::default()
}

/// Get difference between package and filesystem
#[derive(Parser)]
#[clap(author, version, about)]
struct Cli {
    #[clap(subcommand)]
    action: Action,
}

#[derive(clap::Args)]
pub struct CompletionsArgs {
    shell: clap_complete::Shell,
}

#[derive(clap::Args)]
pub struct RunArgs {
    #[clap(short, long)]
    config: Option<PathBuf>,

    #[clap(flatten)]
    flags: Flags,
}

#[derive(clap::Subcommand)]
enum Action {
    Run(RunArgs),
    DumpConfig(RunArgs),
    Completions(CompletionsArgs),
}

fn load_setting(args: &RunArgs) -> Result<Settings> {
    use toml::value::Table;
    use toml::Value as Toml;

    let mut settings = toml::Value::try_from(Settings::default())
        .unwrap()
        .try_into::<Table>()
        .unwrap();

    if let Some(config) = &args.config {
        let config = std::fs::read_to_string(config)
            .with_context(|| format!("failed to read {:?}", config))?
            .parse::<toml::Value>()
            .with_context(|| format!("failed to parse {:?}", config))?
            .try_into::<Table>()?;
        settings.extend(config);
    }

    let args = Toml::try_from(&args.flags).unwrap().try_into::<Table>()?;
    settings.extend(args);

    Ok(Toml::Table(settings).try_into()?)
}

fn print_diff(expected: &str, actual: &str) {
    struct LineNo(Option<usize>);

    use std::fmt;
    impl fmt::Display for LineNo {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.0 {
                None => write!(f, "    "),
                Some(idx) => write!(f, "{:<4}", idx + 1),
            }
        }
    }

    let diff = similar::TextDiff::from_lines(expected, actual);
    for (_, group) in diff.grouped_ops(3).iter().enumerate() {
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    similar::ChangeTag::Delete => ("-", Style::new().red()),
                    similar::ChangeTag::Insert => ("+", Style::new().green()),
                    similar::ChangeTag::Equal => (" ", Style::new().dim()),
                };
                print!("{}", s.apply_to(sign));
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        print!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        print!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    println!();
                }
            }
        }
    }
}

fn main() -> Result<()> {
    env_logger::init_from_env(
        env_logger::Env::default().filter_or(env_logger::DEFAULT_FILTER_ENV, "info"),
    );

    let args = Cli::parse();

    let settings = match args.action {
        Action::DumpConfig(runargs) => {
            let settings = load_setting(&runargs)?;
            println!("{}", toml::to_string(&settings)?);
            return Ok(());
        }
        Action::Completions(shell) => {
            use clap::IntoApp;
            let mut app = Cli::into_app();
            let appname = app.get_name().to_string();
            clap_complete::generate(shell.shell, &mut app, appname, &mut std::io::stdout());
            return Ok(());
        }
        Action::Run(runargs) => load_setting(&runargs)?,
    };
    log::debug!("Settings: {:?}", settings);

    let (pkgs, diffs, tracked) = diff::gen_diff(&settings)?;

    for (path, diff) in &diffs {
        match diff {
            Diff::Missing(content) => {
                println!("{}", style(format!("{}: {}", diff, p2s(path)?,)).red(),);
                match content {
                    Content::File(FileContent::Text(expected)) => {
                        if settings.flags.show_text_diff {
                            print_diff(&expected, "");
                        }
                    }
                    Content::Symlink(expected) => {
                        if settings.flags.show_text_diff {
                            print_diff(p2s(&expected)?, "");
                        }
                    }
                    _ => (),
                }
            }
            Diff::Unmanaged(unmanaged, _) => {
                println!("{}", style(format!("{}: {}", diff, p2s(path)?,)).green(),);
                match unmanaged {
                    UnmanagedDiff::Single(Content::File(FileContent::Text(actual))) => {
                        if settings.flags.show_text_diff {
                            print_diff("", &actual);
                        }
                    }
                    UnmanagedDiff::Single(Content::Symlink(actual)) => {
                        if settings.flags.show_text_diff {
                            print_diff("", &p2s(actual)?);
                        }
                    }

                    _ => (),
                }
            }
            Diff::Ignored(_) => {
                if settings.flags.show_ignored {
                    println!("{}", style(format!("{}: {}", diff, p2s(path)?)));
                }
            }
            Diff::Managed(_, managed_diff) => {
                println!("{}", style(format!("{}: {}", diff, p2s(path)?)).yellow());

                if let ManagedDiff::File(Some(ContentDiff::Text(expected, actual))) = managed_diff {
                    if settings.flags.show_text_diff {
                        print_diff(expected, actual);
                    }
                }
            }
        }
    }

    let mut modified = 0;
    let mut missing = 0;
    let mut unmanaged = 0;
    let mut ignored = 0;
    for (_, diff) in &diffs {
        match diff {
            Diff::Missing(_) => {
                missing += 1;
            }
            Diff::Unmanaged(..) => {
                unmanaged += 1;
            }
            Diff::Ignored(_) => {
                ignored += 1;
            }
            Diff::Managed(..) => {
                modified += 1;
            }
        }
    }
    println!("tracked: {}", tracked);
    println!("modified: {}", modified);
    println!("missing: {}", missing);
    println!("unmanaged: {}", unmanaged);
    println!("ignored: {}", ignored);

    if let Some(outdir) = &settings.flags.outdir {
        let backup_sh = outdir.join("backup.sh");
        let restore_sh = outdir.join("restore.sh");

        use std::io::Write;
        log::info!("Writing {:?}...", backup_sh);
        let data = create_scripts::write_backup(&pkgs, &diffs)
            .with_context(|| format!("generating {:?} failed", backup_sh))?;
        let mut file = std::fs::File::create(&backup_sh)
            .with_context(|| format!("opening {:?} failed", backup_sh))?;
        file.write(data.as_bytes())?;

        log::info!("Writing {:?}...", restore_sh);
        let data = create_scripts::write_restore(&diffs)
            .with_context(|| format!("generating  {:?} failed", restore_sh))?;
        let mut file = std::fs::File::create(&restore_sh)
            .with_context(|| format!("opening {:?} failed", restore_sh))?;
        file.write(data.as_bytes())?;
    }
    Ok(())
}
