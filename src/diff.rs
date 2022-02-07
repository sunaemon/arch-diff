use super::ffi;
use super::types::*;
use super::util::*;
use super::Settings;
use anyhow::{anyhow, Context, Result};
use either::Either;
use handlebars::handlebars_helper;
use rayon::prelude::*;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};

struct GetPkgLocation {
    candidates: HashMap<String, Vec<PathBuf>>,
}

impl GetPkgLocation {
    fn new(env_info: &EnvInfo, settings: &Settings) -> Result<GetPkgLocation> {
        let mut candidates = HashMap::new();

        for dir in &settings.cachedir {
            let dir = PathBuf::from(render(dir, env_info)?);
            if !dir.is_dir() {
                continue;
            }
            for entry in walkdir::WalkDir::new(dir) {
                let entry = entry?;
                let path = entry.path();
                let filename = entry
                    .file_name()
                    .to_str()
                    .ok_or_else(|| anyhow!("Cannot parse filename as UTF-8: {:?}", entry))?;
                for ext in &settings.exts {
                    if filename.ends_with(ext) {
                        let stem = filename.strip_suffix(ext).unwrap().to_string();

                        candidates
                            .entry(stem)
                            .or_insert(Vec::new())
                            .push(path.to_path_buf())
                    }
                }
            }
        }

        Ok(GetPkgLocation { candidates })
    }

    fn get(&self, pkg: &alpm::Package) -> Result<PathBuf> {
        let pkg = &format!(
            "{}-{}-{}",
            pkg.name(),
            pkg.version(),
            pkg.arch()
                .ok_or(anyhow!("arch is empty for: {}", pkg.name()))?
        );

        match self.candidates.get(pkg) {
            Some(paths) => match &paths[..] {
                [path] => Ok(path.to_path_buf()),
                _ => Err(anyhow!("multipe package find for {}: {:?}", pkg, paths)),
            },
            None => Err(anyhow!("cannot find package: {}", &pkg)),
        }
    }
}

#[derive(Serialize, Debug)]
struct PkgInfo {
    version: String,
    arch: Option<String>,
    file: PathBuf,
}

#[derive(Serialize, Debug)]
struct EnvInfo {
    package_info: BTreeMap<String, PkgInfo>,
    home: Option<PathBuf>,
    cache: Option<PathBuf>,
}

handlebars_helper!(replace: |s: str, pattern: str, to:str| s.to_string().replace(pattern, to));

fn render(template: &str, env_info: &EnvInfo) -> Result<String> {
    let mut hbs = handlebars::Handlebars::new();
    hbs.register_helper("replace", Box::new(replace));
    Ok(hbs.render_template(template, &serde_json::json!(env_info))?)
}

fn get_entries(settings: &Settings) -> Result<(HashMap<PathBuf, MTreeEntryData>, EnvInfo)> {
    use libarchive::archive::Entry;

    let handle =
        alpm::Alpm::new(p2s(&settings.rootdir)?, p2s(&settings.dbpath)?).with_context(|| {
            format!(
                "open handler of alpm with {:?}, {:?} failed",
                settings.rootdir, settings.dbpath
            )
        })?;

    handle.set_log_cb((), |level, msg, _| match level {
        alpm::LogLevel::ERROR => log::error!("{}", msg.trim()),
        alpm::LogLevel::WARNING => log::warn!("{}", msg.trim()),
        alpm::LogLevel::DEBUG => log::debug!("{}", msg.trim()),
        alpm::LogLevel::FUNCTION => log::debug!("{}", msg.trim()),
        _ => panic!("unknown Alpm Loglevel: {:?}", level),
    });

    log::info!("Collecting pkgs...");

    let mut env_info = EnvInfo {
        package_info: BTreeMap::new(),
        home: dirs::home_dir(),
        cache: dirs::cache_dir(),
    };

    let pkg_candidates = GetPkgLocation::new(&env_info, &settings)?;

    log::info!("Reading mtree...");
    let mut entries = HashMap::new();
    for pkg in handle.localdb().pkgs() {
        log::debug!("Reading pkg: {:?}", pkg);

        let name = pkg.name().to_string();
        let version = pkg.version().to_string();
        let arch = pkg.arch().map(|arch| arch.to_string());
        let file = pkg_candidates.get(&pkg)?;

        env_info.package_info.insert(
            name.to_string(),
            PkgInfo {
                version,
                arch,
                file,
            },
        );

        for file in pkg.mtree()? {
            let path = {
                let path = file.pathname();
                let path = path.strip_prefix("./").ok_or(anyhow!(
                    "File name fiels in mtree does not starts from ./: {:?} in pkg: {}",
                    path,
                    pkg.name()
                ))?;
                if path.starts_with(".") {
                    continue;
                };
                settings.rootdir.join(Path::new(path))
            };
            let content = match FileType::try_from(file.filetype())? {
                FileType::Directory => ContentSummary::Directory,
                FileType::File => ContentSummary::File(
                    ffi::get_sha256(&file)
                        .ok_or(anyhow!(
                            "Failed to get SHA256 sum for {:?} in package {}",
                            file.pathname(),
                            pkg.name(),
                        ))?
                        .clone(),
                ),
                FileType::Symlink => {
                    ContentSummary::Symlink(Path::new(file.symlink()).to_path_buf())
                }
            };
            let entry = MTreeEntryData {
                content,
                mtime: unsafe { libarchive3_sys::ffi::archive_entry_mtime(file.entry()) },
                uid: unsafe { libarchive3_sys::ffi::archive_entry_uid(file.entry()) }.try_into()?,
                gid: unsafe { libarchive3_sys::ffi::archive_entry_gid(file.entry()) }.try_into()?,
                mode: unsafe { libarchive3_sys::ffi::archive_entry_mode(file.entry()) },
                package: name.to_string(),
            };
            entries.insert(path, entry);
        }
    }

    Ok((entries, env_info))
}

fn handle_permission_denied<T, F, C>(
    r: Result<T, std::io::Error>,
    settings: &Settings,
    context: F,
) -> Result<Option<T>>
where
    C: std::fmt::Display + Send + Sync + 'static,
    F: FnOnce() -> C,
{
    match r {
        Ok(v) => Ok(Some(v)),
        Err(err) => {
            if err.kind() == ErrorKind::PermissionDenied {
                if settings.flags.ignore_permission_denied {
                    log::warn!("{} (Premission denied)", context());
                    Ok(None)
                } else {
                    Err(err)
                        .with_context(|| {
                            "Please add `--ignore-permission-denied` or execute with `sudo -E`"
                        })
                        .with_context(context)
                }
            } else {
                Err(err).with_context(context)
            }
        }
    }
}

struct IsIgnored {
    ignore: Vec<String>,
    mountinfo: Vec<procfs::process::MountInfo>,
    settings: Settings,
}

impl IsIgnored {
    fn new(
        settings: &Settings,
        mountinfo: Vec<procfs::process::MountInfo>,
        env_info: &EnvInfo,
    ) -> Result<Self> {
        let ignore = settings
            .ignore
            .iter()
            .map(|p| render(p, env_info))
            .collect::<Result<_>>()?;

        Ok(IsIgnored {
            ignore,
            mountinfo,
            settings: (*settings).clone(),
        })
    }

    fn check(&self, path: &Path) -> Option<IgnoredReason> {
        let fstype = get_fstype(&self.mountinfo, path);
        for fs in &self.settings.ignorefs {
            if fs == fstype {
                return Some(IgnoredReason::ByFilesystem(fstype.to_string()));
            }
        }

        for pattern in &self.ignore {
            if path.starts_with(pattern) {
                return Some(IgnoredReason::ByPattern(fstype.to_string()));
            }
        }

        None
    }
}

struct Defer {
    common_diff: CommonDiff,
    hash_expected: [u8; 32],
    package: String,
}

fn diff_direntry(
    p: std::fs::DirEntry,
    e: MTreeEntryData,
    settings: &Settings,
) -> Result<Either<Diff, Defer>> {
    let path = p.path();
    let metadata = p.metadata()?;

    use std::os::unix::fs::MetadataExt;

    let mode_mask = 0o7777;
    let common_diff = CommonDiff {
        mode: if mode_mask & metadata.mode() != mode_mask & e.mode {
            Some((mode_mask & metadata.mode(), mode_mask & e.mode))
        } else {
            None
        },
        uid: if metadata.uid() != e.uid {
            Some((metadata.uid(), e.uid))
        } else {
            None
        },
        gid: if metadata.gid() != e.gid {
            Some((metadata.gid(), e.gid))
        } else {
            None
        },
        mtime: if metadata.mtime() != e.mtime && settings.flags.check_mtime {
            Some((metadata.mtime(), e.mtime))
        } else {
            None
        },
    };

    let filetype_expected = FileType::from(&e.content);
    let filetype_actual = FileType::try_from(metadata.file_type())?;
    let diff = if filetype_expected != filetype_actual {
        ManagedDiff::FileType(filetype_expected, filetype_actual)
    } else {
        match e.content {
            ContentSummary::Directory => ManagedDiff::Directory,
            ContentSummary::File(hash_expected) => {
                return Ok(Either::Right(Defer {
                    common_diff,
                    hash_expected,
                    package: e.package,
                }))
            }
            ContentSummary::Symlink(target_path_expected) => {
                let target_path_actual = std::fs::read_link(&path)
                    .with_context(|| format!("readlink  of {:?} failed", &path))?;
                let diff = if &target_path_actual != &target_path_expected {
                    Some((target_path_actual, target_path_expected))
                } else {
                    None
                };
                ManagedDiff::Symlink(diff)
            }
        }
    };

    Ok(Either::Left(Diff::Managed(common_diff, diff)))
}

#[derive(Clone)]
enum FileDiff {
    Same,
    Binary,
    Text { package: String },
    Ignored(IgnoredReason),
}

fn calc_file_diff(
    jobs: Vec<(PathBuf, Defer)>,
    settings: &Settings,
) -> Result<Vec<(PathBuf, CommonDiff, FileDiff)>> {
    jobs.into_par_iter()
        .map(|(path, defer)| -> Result<_> {
            let Defer {
                common_diff,
                hash_expected,
                package,
            } = defer;
            let new_diff = {
                match handle_permission_denied(calc_sha256(&path), settings, || {
                    format!("calculation of SHA256 of {:?} failed", path)
                })? {
                    None => (
                        path,
                        common_diff,
                        FileDiff::Ignored(IgnoredReason::PermissionDenied),
                    ),
                    Some(hash_actual) => {
                        if hash_expected == hash_actual {
                            (path, common_diff, FileDiff::Same)
                        } else {
                            if is_binary_file(&path).with_context(|| {
                                format!("binary file detection of {:?} failed", &path)
                            })? {
                                (path, common_diff, FileDiff::Binary)
                            } else {
                                (
                                    path.to_path_buf(),
                                    common_diff,
                                    FileDiff::Text {
                                        package: package.to_string(),
                                    },
                                )
                            }
                        }
                    }
                }
            };
            Ok(new_diff)
        })
        .collect::<Result<_>>()
}

fn fetch_contents(
    jobs: &HashMap<String, Vec<(PathBuf, CommonDiff)>>,
    env_info: &EnvInfo,
    settings: &Settings,
) -> Result<Vec<(PathBuf, CommonDiff, FileContent)>> {
    jobs.par_iter()
        .map(|(pkg, diffs)| -> Result<_> {
            let mut text_diffs = Vec::new();
            let pkgfile = &env_info.package_info[pkg].file;
            let mut builder = libarchive::reader::Builder::new();
            builder
                .support_filter(libarchive::archive::ReadFilter::All)
                .context("support_filter failed")?;
            builder
                .support_format(libarchive::archive::ReadFormat::Tar)
                .context("support_format failed")?;

            let mut reader = libarchive::reader::FileReader::open(builder, pkgfile)
                .with_context(|| format!("open {:?} failed", pkgfile))?;
            loop {
                use libarchive::archive::Entry;
                use libarchive::reader::Reader;
                let e = match reader.next_header() {
                    None => break,
                    Some(e) => e,
                };
                let path = settings.rootdir.join(e.pathname());
                if let Some((_, common_diff)) = diffs.iter().find(|(file, _)| *file == path) {
                    let diff = if e.size() > settings.max_text_size.into() {
                        FileContent::TooBigText
                    } else {
                        let mut data = Vec::new();
                        let eof = loop {
                            match reader.read_block().with_context(|| {
                                format!("read {:?} from {:?} failed", path, pkgfile)
                            })? {
                                None => break true,
                                Some(b) => {
                                    if data.len() + b.len()
                                        > settings.max_text_size.try_into().unwrap()
                                    {
                                        break false;
                                    }

                                    data.extend(b)
                                }
                            }
                        };

                        match String::from_utf8(data) {
                            Ok(data) => {
                                if eof {
                                    FileContent::Text(data)
                                } else {
                                    FileContent::TooBigText
                                }
                            }
                            Err(_) => FileContent::Binary,
                        }
                    };
                    text_diffs.push((path, common_diff.clone(), diff))
                }
            }
            Ok(text_diffs)
        })
        .try_reduce(|| Vec::new(), |a, b| Ok([a, b].concat()))
}

fn read_content(path: &Path, settings: &Settings) -> Result<Option<Content>> {
    use std::fs::File;
    use std::io::Read;
    use std::os::unix::fs::MetadataExt;

    let metainfo = match std::fs::symlink_metadata(path) {
        Ok(metainfo) => metainfo,
        Err(err) => {
            if err.kind() == ErrorKind::NotFound {
                return Ok(None);
            }
            return Err(err).with_context(|| format!("lstat of {:?} failed", &path));
        }
    };

    let filetype = match FileType::try_from(metainfo.file_type()) {
        Ok(filetype) => filetype,
        Err(_) => return Ok(Some(Content::Other)),
    };

    let content_diff = match filetype {
        FileType::Directory => Content::Directory,
        FileType::File => {
            let file_content = if path.metadata()?.size() > settings.max_text_size.into() {
                if is_binary_file(&path)? {
                    FileContent::Binary
                } else {
                    FileContent::TooBigText
                }
            } else {
                let mut f = File::open(&path)?;
                let mut data = Vec::new();
                f.read_to_end(&mut data)?;
                match String::from_utf8(data) {
                    Ok(expected) => FileContent::Text(expected),
                    Err(_) => FileContent::Binary,
                }
            };
            Content::File(file_content)
        }
        FileType::Symlink => Content::Symlink(
            std::fs::read_link(&path)
                .with_context(|| format!("readlink  of {:?} failed", &path))?,
        ),
    };

    Ok(Some(content_diff))
}

fn calc_diff(
    mut entries: HashMap<PathBuf, MTreeEntryData>,
    env_info: &EnvInfo,
    settings: &Settings,
) -> Result<(BTreeMap<PathBuf, Diff>, usize)> {
    let mut diffs = BTreeMap::new();

    let mut tracked = 0;

    let procinfo = procfs::process::Process::myself()?;
    let mountinfo = procinfo.mountinfo()?;
    let is_ignored = IsIgnored::new(settings, mountinfo, env_info)?;

    log::info!("Checking metainfo...");

    let mut dirs = vec![settings.rootdir.to_path_buf()];
    let mut jobs: Vec<(PathBuf, Defer)> = Vec::new();
    let mut unmanaged_entries = BTreeSet::new();
    loop {
        let next = match dirs.pop() {
            None => break,
            Some(next) => next,
        };
        let read_dirs = match handle_permission_denied(std::fs::read_dir(&next), settings, || {
            format!("read for {:?} failed", &next)
        })? {
            None => {
                diffs.insert(next, Diff::Ignored(IgnoredReason::PermissionDenied));
                continue;
            }
            Some(read_dirs) => read_dirs,
        };
        for p in read_dirs {
            let p = p.with_context(|| format!("Read metadata under {:?} failed", &next))?;
            let path = p.path().to_path_buf();

            if let Some(ignore_reason) = is_ignored.check(&path) {
                entries.retain(|p, _| !p.starts_with(&path));
                diffs.insert(path, Diff::Ignored(ignore_reason));
                continue;
            }
            tracked += 1;
            let diff = match entries.remove(&path) {
                None => {
                    unmanaged_entries.insert(path);
                    continue;
                }
                Some(e) => match diff_direntry(p, e, settings)
                    .with_context(|| format!("diff Calculation for {:?} failed", &path))?
                {
                    Either::Left(diff) => diff,
                    Either::Right(defer) => {
                        jobs.push((path, defer));
                        continue;
                    }
                },
            };

            match &diff {
                Diff::Managed(common_diff, ManagedDiff::Directory) => {
                    dirs.push(path.to_path_buf());
                    if *common_diff != CommonDiff::default() {
                        diffs.insert(path, diff);
                    }
                }
                Diff::Managed(common_diff, ManagedDiff::File(None))
                | Diff::Managed(common_diff, ManagedDiff::Symlink(None)) => {
                    if *common_diff != CommonDiff::default() {
                        diffs.insert(path, diff);
                    }
                }
                _ => {
                    diffs.insert(path, diff);
                }
            }
        }
    }

    log::info!("Checking file diff...");
    let mut text_diff_jobs: HashMap<String, Vec<(PathBuf, CommonDiff)>> = HashMap::new();
    for (path, common_diff, file_diff) in calc_file_diff(jobs, settings)? {
        match (common_diff, file_diff) {
            (common_diff, FileDiff::Binary) => {
                diffs.insert(
                    path,
                    Diff::Managed(common_diff, ManagedDiff::File(Some(ContentDiff::Binary))),
                );
            }
            (common_diff, FileDiff::Text { package }) => text_diff_jobs
                .entry(package.to_string())
                .or_insert(Vec::new())
                .push((path, common_diff)),
            (_, FileDiff::Ignored(reason)) => {
                diffs.insert(path, Diff::Ignored(reason));
            }
            (common_diff, FileDiff::Same) => {
                if common_diff != CommonDiff::default() {
                    diffs.insert(path, Diff::Managed(common_diff, ManagedDiff::File(None)));
                }
            }
        }
    }
    for (path, e) in entries.into_iter() {
        text_diff_jobs
            .entry(e.package.to_string())
            .or_insert(Vec::new())
            .push((path, CommonDiff::default()));
    }

    log::info!("Generating text diff...");
    for (path, common_diff, expected) in fetch_contents(&text_diff_jobs, &env_info, settings)? {
        let actual = match read_content(&path, settings)? {
            Some(Content::File(file_content)) => Some(file_content),
            None => None,
            _ => panic!(),
        };
        let diff = match (expected, actual) {
            (expected, None) => Diff::Missing(Content::File(expected)),
            (FileContent::Text(expected), Some(FileContent::Text(actual))) => Diff::Managed(
                common_diff,
                ManagedDiff::File(Some(ContentDiff::Text(expected, actual))),
            ),
            (FileContent::Binary, _) | (_, Some(FileContent::Binary)) => {
                Diff::Managed(common_diff, ManagedDiff::File(Some(ContentDiff::Binary)))
            }
            (FileContent::TooBigText, _) | (_, Some(FileContent::TooBigText)) => Diff::Managed(
                common_diff,
                ManagedDiff::File(Some(ContentDiff::TextTooBig)),
            ),
        };

        diffs.insert(path, diff);
    }

    {
        let unmanaged_entries_parents = unmanaged_entries
            .iter()
            .map(|e| e.parent().unwrap().to_path_buf())
            .collect::<HashSet<_>>();

        let mut unmanaged_entries_parents_merged = BTreeSet::new();
        for unmanaged_entries_parent in unmanaged_entries_parents {
            fn get_childs(path: &Path) -> Result<HashSet<PathBuf>, std::io::Error> {
                std::fs::read_dir(&path)?
                    .map(|d| Ok(d?.path().to_path_buf()))
                    .collect()
            }
            let childs_actual = match handle_permission_denied(
                get_childs(&unmanaged_entries_parent),
                settings,
                || format!("get child entries of {:?} failed", unmanaged_entries_parent),
            )? {
                None => continue,
                Some(childs_actual) => childs_actual,
            };
            let childs_untracked = unmanaged_entries
                .iter()
                .filter(|f| f.starts_with(&unmanaged_entries_parent))
                .map(ToOwned::to_owned)
                .collect::<HashSet<PathBuf>>();

            let diff = childs_actual
                .symmetric_difference(&childs_untracked)
                .collect::<Vec<&PathBuf>>();

            if !diff.is_empty() {
                log::debug!("{:?} unmached, {:?}", unmanaged_entries_parent, diff);
            } else {
                log::debug!("{:?} mached", unmanaged_entries_parent);
                if diff.len() > settings.min_merge_file.try_into().unwrap() {
                    unmanaged_entries_parents_merged.insert(unmanaged_entries_parent);
                }
            }
        }

        fn get_entry_num(p: &Path) -> Result<UnmanagedStats, std::io::Error> {
            let mut entries = 0;
            let mut size = 0;
            for entry in walkdir::WalkDir::new(p) {
                entries += 1;
                size += entry?.metadata()?.len()
            }
            Ok(UnmanagedStats { entries, size })
        }

        unmanaged_entries.retain(|path| {
            unmanaged_entries_parents_merged
                .iter()
                .all(|p| !path.starts_with(p))
        });

        for path in &unmanaged_entries {
            if path.is_dir() {
                let stats = get_entry_num(&path).unwrap();
                if stats.entries < settings.min_merge_file.into() {
                    for entry in walkdir::WalkDir::new(path) {
                        let entry = entry.unwrap();
                        let content = read_content(entry.path(), settings).unwrap().unwrap();
                        diffs.insert(
                            entry.path().to_path_buf(),
                            Diff::Unmanaged(UnmanagedDiff::Single(content), None),
                        );
                    }
                } else {
                    diffs.insert(
                        path.to_path_buf(),
                        Diff::Unmanaged(UnmanagedDiff::Single(Content::Directory), Some(stats)),
                    );
                }
            } else {
                let content = read_content(&path, settings).unwrap().unwrap();
                diffs.insert(
                    path.to_path_buf(),
                    Diff::Unmanaged(UnmanagedDiff::Single(content), None),
                );
            }
        }

        for path in &unmanaged_entries_parents_merged {
            let stats = get_entry_num(&path).unwrap();
            diffs.insert(
                path.to_path_buf(),
                Diff::Unmanaged(UnmanagedDiff::All, Some(stats)),
            );
        }
    }

    Ok((diffs, tracked))
}

pub fn gen_diff(
    settings: &Settings,
) -> Result<(BTreeSet<PathBuf>, BTreeMap<PathBuf, Diff>, usize)> {
    log::info!("Getting entries...");

    let (entries, env_info) = get_entries(settings)?;

    log::info!("Generating diff...");

    let (diffs, tracked) = calc_diff(entries, &env_info, settings)?;

    Ok((
        env_info
            .package_info
            .into_values()
            .map(|info| info.file)
            .collect::<BTreeSet<_>>(),
        diffs,
        tracked,
    ))
}
