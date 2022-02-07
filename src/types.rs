use crate::util::*;
use anyhow::{anyhow, Result};
use libc::{mode_t, time_t};
use std::path::PathBuf;

#[derive(Debug, Eq, PartialEq)]
pub enum FileType {
    Directory,
    File,
    Symlink,
}

impl std::fmt::Display for FileType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            FileType::Directory => write!(f, "Directory"),
            FileType::File => write!(f, "File"),
            FileType::Symlink => write!(f, "Symlink"),
        }
    }
}

impl From<&ContentSummary> for FileType {
    fn from(v: &ContentSummary) -> Self {
        match v {
            ContentSummary::Directory => FileType::Directory,
            ContentSummary::File(_) => FileType::File,
            ContentSummary::Symlink(_) => FileType::Symlink,
        }
    }
}

#[derive(Debug)]
pub enum ContentSummary {
    Directory,
    File([u8; 32]),
    Symlink(PathBuf),
}

#[derive(Debug)]
pub struct MTreeEntryData {
    pub package: String,
    pub mode: mode_t,
    pub uid: u32,
    pub gid: u32,
    pub mtime: time_t,
    pub content: ContentSummary,
}

impl TryFrom<std::fs::FileType> for FileType {
    type Error = anyhow::Error;
    fn try_from(v: std::fs::FileType) -> Result<Self> {
        if v.is_dir() {
            Ok(FileType::Directory)
        } else if v.is_file() {
            Ok(FileType::File)
        } else if v.is_symlink() {
            Ok(FileType::Symlink)
        } else {
            Err(anyhow!("std::fs::Filetype {:?} is not supported", v))
        }
    }
}

impl TryFrom<libarchive::archive::FileType> for FileType {
    type Error = anyhow::Error;
    fn try_from(v: libarchive::archive::FileType) -> Result<Self> {
        match v {
            libarchive::archive::FileType::Directory => Ok(FileType::Directory),
            libarchive::archive::FileType::RegularFile => Ok(FileType::File),
            libarchive::archive::FileType::SymbolicLink => Ok(FileType::Symlink),
            _ => Err(anyhow!(
                "libarchive::archive::Filetype {} is not supported",
                v as u32
            )),
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct CommonDiff {
    pub uid: Option<(u32, u32)>,
    pub gid: Option<(u32, u32)>,
    pub mode: Option<(mode_t, mode_t)>,
    pub mtime: Option<(i64, i64)>,
}

#[derive(Debug, Clone)]
pub enum ContentDiff {
    Text(String, String),
    TextTooBig,
    Binary,
}

#[derive(Debug, Clone)]
pub enum FileContent {
    Text(String),
    TooBigText,
    Binary,
}

#[derive(Debug, Clone)]
pub enum Content {
    Directory,
    File(FileContent),
    Symlink(PathBuf),
    Other,
}

impl std::fmt::Display for Content {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Content::Directory => write!(f, "a directory"),
            Content::File(FileContent::Text(_)) => write!(f, "a text file"),
            Content::File(FileContent::TooBigText) => write!(f, "a text file too big to show"),
            Content::File(FileContent::Binary) => write!(f, "a binary file"),
            Content::Other => write!(f, "unsupported entry"),
            Content::Symlink(_) => write!(f, "a symlink file"),
        }
    }
}

#[derive(Debug)]
pub enum ManagedDiff {
    FileType(FileType, FileType),
    Directory,
    File(Option<ContentDiff>),
    Symlink(Option<(PathBuf, PathBuf)>),
}

#[derive(Debug)]
pub enum UnmanagedDiff {
    Single(Content),
    All,
}

#[derive(Debug)]
pub struct UnmanagedStats {
    pub entries: u64,
    pub size: u64,
}

impl std::fmt::Display for UnmanagedStats {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} entries {}",
            self.entries,
            human_bytes::human_bytes(self.size as f64)
        )
    }
}

#[derive(Debug, Clone)]
pub enum IgnoredReason {
    ByFilesystem(String),
    ByPattern(String),
    PermissionDenied,
}

impl std::fmt::Display for IgnoredReason {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            IgnoredReason::ByFilesystem(filesystem) => write!(f, "filesystem: {}", filesystem),
            IgnoredReason::ByPattern(pattern) => write!(f, "pattern: {}", pattern),
            IgnoredReason::PermissionDenied => write!(f, "permission denied"),
        }
    }
}

#[derive(Debug)]
pub enum Diff {
    Ignored(IgnoredReason),
    Unmanaged(UnmanagedDiff, Option<UnmanagedStats>),
    Missing(Content),
    Managed(CommonDiff, ManagedDiff),
}

impl std::fmt::Display for Diff {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Diff::Missing(content) => {
                write!(f, "[missing] {}", content)
            }
            Diff::Unmanaged(UnmanagedDiff::Single(content), None) => {
                write!(f, "[unmanaged] {}", content)
            }
            Diff::Unmanaged(_, Some(stats)) => {
                write!(f, "[unmanaged] {}", stats)
            }
            Diff::Unmanaged(_, None) => {
                write!(f, "[unmanaged]")
            }
            Diff::Ignored(reason) => {
                write!(f, "[ignored] because {}", reason)
            }
            Diff::Managed(common_diff, managed_diff) => {
                let mut diffs = Vec::new();
                if let Some((expected, actual)) = common_diff.uid {
                    diffs.push(format!("uid: {} -> {}", expected, actual))
                }
                if let Some((expected, actual)) = common_diff.gid {
                    diffs.push(format!("gid: {} -> {}", expected, actual))
                }
                if let Some((expected, actual)) = common_diff.mode {
                    diffs.push(format!("mode: {:o} -> {:o}", expected, actual))
                }
                if let Some((expected, actual)) = common_diff.mtime {
                    diffs.push(format!("mtime: {} -> {}", expected, actual))
                }

                match managed_diff {
                    ManagedDiff::FileType(expected, actual) => {
                        diffs.push(format!("filetype: {} -> {}", expected, actual))
                    }
                    ManagedDiff::Directory => {}
                    ManagedDiff::File(Some(ContentDiff::Binary)) => {
                        diffs.push("binary file changed".to_string())
                    }
                    ManagedDiff::File(Some(ContentDiff::Text(..))) => {
                        diffs.push("text file changed".to_string());
                    }
                    ManagedDiff::File(Some(ContentDiff::TextTooBig)) => {
                        diffs.push("text file to big to show diff changed".to_string());
                    }
                    ManagedDiff::File(None) => {}
                    ManagedDiff::Symlink(Some((expected, actual))) => diffs.push(format!(
                        "symlink: {} -> {}",
                        p2s(expected).unwrap(),
                        p2s(actual).unwrap()
                    )),
                    ManagedDiff::Symlink(None) => {}
                }

                write!(f, "[modified] {}", diffs.join(", "))
            }
        }
    }
}
