use anyhow::Result;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use crate::types::*;
use crate::util::*;

fn strip_root(p: &Path) -> &Path {
    p.strip_prefix("/").unwrap()
}

fn dedup_mkdir(dirs: &BTreeSet<PathBuf>) -> BTreeSet<PathBuf> {
    let mut ret = BTreeSet::new();
    for dir in dirs {
        if dirs.iter().all(|d| !d.starts_with(dir) || dir == d) {
            ret.insert(dir.to_owned());
        }
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dedup_mkdir() {
        let mut dirs = BTreeSet::new();
        dirs.insert(Path::new("a/").to_owned());
        dirs.insert(Path::new("a/a").to_owned());
        dirs.insert(Path::new("a/b/c").to_owned());

        let mut dirs_expect = BTreeSet::new();
        dirs_expect.insert(Path::new("a/a").to_owned());
        dirs_expect.insert(Path::new("a/b/c").to_owned());

        assert_eq!(dedup_mkdir(&dirs), dirs_expect);
    }
}

pub fn write_backup(pkgs: &BTreeSet<PathBuf>, diffs: &BTreeMap<PathBuf, Diff>) -> Result<String> {
    use std::fmt::Write;
    let mut file = String::new();
    write!(
        file,
        r#"#!/bin/bash
set -eu
function usage
{{
    echo "usage: $0 TARGET" >&2
    echo "Create backup in TARGET" >&2
}}
shopt -s dotglob
if [ "$#" -ne 1 ]; then
    usage
    exit 1
fi

readonly backup=$1
"#
    )?;

    let pkgdir = PathBuf::from("$backup/pkg");
    let unmanageddir = PathBuf::from("$backup/unmanaged");

    writeln!(file, "rm -rf {}", p2s(&pkgdir)?)?;
    writeln!(file, "mkdir -p {}", p2s(&pkgdir)?)?;
    for pkg in pkgs {
        let pkgfile = pkg.file_name().unwrap();
        let target_path = pkgdir.join(pkgfile);
        writeln!(file, "cp {} {}", p2s(pkg)?, p2s(&target_path)?)?;
    }

    writeln!(file, "rm -rf {}", p2s(&unmanageddir)?)?;

    let mut new_dirs = BTreeSet::new();
    for (path, diff) in diffs {
        let unmanaged_path = unmanageddir.join(strip_root(&path));
        match diff {
            Diff::Unmanaged(UnmanagedDiff::All, _) => {
                new_dirs.insert(unmanaged_path.to_path_buf());
            }
            Diff::Unmanaged(UnmanagedDiff::Single(Content::Directory), _)
            | Diff::Unmanaged(UnmanagedDiff::Single(Content::File(FileContent::TooBigText)), _)
            | Diff::Unmanaged(UnmanagedDiff::Single(Content::File(FileContent::Binary)), _)
            | Diff::Unmanaged(UnmanagedDiff::Single(Content::Symlink(_)), _)
            | Diff::Unmanaged(UnmanagedDiff::Single(Content::Other), _)
            | Diff::Managed(_, ManagedDiff::File(Some(ContentDiff::Binary)))
            | Diff::Managed(_, ManagedDiff::FileType(..)) => {
                new_dirs.insert(unmanaged_path.parent().unwrap().to_path_buf());
            }
            _ => (),
        }
    }

    for dir in dedup_mkdir(&new_dirs) {
        writeln!(file, "mkdir -p {}", p2s(&dir)?)?;
    }

    for (path, diff) in diffs {
        let stats = match diff {
            Diff::Unmanaged(_, Some(stats)) => format!(" # {}", stats),
            _ => String::new(),
        };
        let cmd = match diff {
            Diff::Unmanaged(UnmanagedDiff::Single(Content::Directory), _)
            | Diff::Managed(_, ManagedDiff::FileType(..)) => {
                Some(format!("cp -rpP \"{}\"", p2s(&path)?,))
            }
            Diff::Unmanaged(UnmanagedDiff::Single(Content::File(FileContent::TooBigText)), _)
            | Diff::Unmanaged(UnmanagedDiff::Single(Content::File(FileContent::Binary)), _)
            | Diff::Managed(_, ManagedDiff::File(Some(ContentDiff::Binary))) => {
                Some(format!("cp -p \"{}\"", p2s(&path)?,))
            }
            Diff::Unmanaged(UnmanagedDiff::Single(Content::Symlink(_)), _) => {
                Some(format!("cp -pP \"{}\"", p2s(&path)?))
            }
            Diff::Unmanaged(UnmanagedDiff::All, _) => {
                Some(format!("cp -rpP \"{}/*\"", p2s(&path)?))
            }
            _ => None,
        };
        if let Some(cmd) = cmd {
            let unmanaged_path = unmanageddir.join(strip_root(&path));
            writeln!(file, "{} \"{}\"{}", cmd, p2s(&unmanaged_path)?, stats)?
        }
    }
    Ok(file)
}

pub fn write_restore(diffs: &BTreeMap<PathBuf, Diff>) -> Result<String> {
    use std::fmt::Write;
    let mut file = String::new();

    let pkgdir = PathBuf::from("$backup/pkg");
    let unmanageddir = PathBuf::from("$backup/unmanaged");
    let new_root = PathBuf::from("$new_root");

    write!(
        file,
        r#"#!/bin/bash
set -eu
function usage
{{
    echo "usage: $0 BACKUP NEW_ROOT" >&2
    echo "Restore backup in BACKUP to NEWROOT" >&2
}}
shopt -s dotglob
if [ "$#" -ne 2 ]; then
    usage
    exit 1
fi

readonly backup=$1
readonly new_root=$2
"#
    )?;

    writeln!(
        file,
        "pacstrap -UMG {} {}/*",
        p2s(&new_root)?,
        p2s(&pkgdir)?
    )?;

    let mut new_dirs = BTreeSet::new();
    for (path, diff) in diffs {
        let target_path = new_root.join(strip_root(&path));
        match diff {
            Diff::Unmanaged(UnmanagedDiff::Single(Content::File(FileContent::Text(_))), _) => {
                new_dirs.insert(target_path.parent().unwrap().to_path_buf());
            }
            _ => (),
        }
    }
    for dir in dedup_mkdir(&new_dirs) {
        writeln!(file, "mkdir -p {}", p2s(&dir)?)?;
    }

    for (path, diff) in diffs {
        let target_path = new_root.join(strip_root(&path));
        match diff {
            Diff::Managed(_, ManagedDiff::File(Some(ContentDiff::Text(expected, actual)))) => {
                let udiff = similar::TextDiff::from_lines(expected, actual)
                    .unified_diff()
                    .context_radius(5)
                    .header(
                        &format!("pkg{}", p2s(&path)?),
                        &format!("fs{}", p2s(&path)?),
                    )
                    .to_string();
                // Note that the beginning of a line in a unified format patch is either a space or a +/-, so the line will never be EOS.
                writeln!(file, "patch {} <<'EOS'\n{}EOS", p2s(&target_path)?, udiff)?
            }
            Diff::Unmanaged(UnmanagedDiff::Single(Content::File(FileContent::Text(actual))), _) => {
                let udiff = similar::TextDiff::from_lines("", actual)
                    .unified_diff()
                    .context_radius(5)
                    .header(
                        &format!("pkg{}", p2s(&path)?),
                        &format!("fs{}", p2s(&path)?),
                    )
                    .to_string();
                writeln!(file, "touch {}", p2s(&target_path)?)?;
                // Note that the beginning of a line in a unified format patch is either a space or a +/-, so the line will never be EOS.
                writeln!(file, "patch {} <<'EOS'\n{}EOS", p2s(&target_path)?, udiff)?
            }
            _ => (),
        }
    }

    for (path, diff) in diffs {
        let target_path = new_root.join(strip_root(&path));
        match diff {
            Diff::Managed(_, ManagedDiff::Symlink(Some((_, actual)))) => {
                writeln!(file, "ln -sf {} {}", p2s(&actual)?, p2s(&target_path)?)?
            }
            _ => (),
        }
    }
    for (path, diff) in diffs {
        let target_path = new_root.join(strip_root(&path));
        match diff {
            Diff::Missing(_) => writeln!(file, "rm {}", p2s(&target_path)?)?,
            Diff::Managed(common, _) => {
                if let Some((_, actual)) = common.uid {
                    writeln!(file, "chown {} {}", actual, p2s(&target_path)?)?
                }
                if let Some((_, actual)) = common.gid {
                    writeln!(file, "chown {} :{}", actual, p2s(&target_path)?)?
                }
                if let Some((_, actual)) = common.mode {
                    writeln!(file, "chmod {:o} {}", actual, p2s(&target_path)?)?
                }
            }
            _ => (),
        }
    }

    writeln!(
        file,
        "cp -rpP {}/* {}",
        p2s(&unmanageddir)?,
        p2s(&new_root)?
    )?;
    Ok(file)
}
