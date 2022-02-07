use anyhow::{anyhow, Result};
use std::path::Path;

pub fn p2s(p: &Path) -> Result<&str> {
    p.to_str()
        .ok_or(anyhow!("path cannot be parsed in UTF-8: {:?}", &p))
}

pub fn calc_sha256<'a>(path: &Path) -> Result<[u8; 32], std::io::Error> {
    use sha2::Digest;

    let mut f = std::fs::File::open(path)?;
    let mut hasher = sha2::Sha256::new();
    std::io::copy(&mut f, &mut hasher)?;
    Ok(<[u8; 32]>::from(hasher.finalize()))
}

pub fn is_binary_file(path: &Path) -> Result<bool, std::io::Error> {
    use std::io::Read;

    let mut buf = vec![0u8; 1024];

    let f = std::fs::File::open(&path)?;
    let mut handle = f.take(1024);
    let n = handle.read(&mut buf)?;

    Ok(buf[0..n].contains(&0))
}

pub fn get_fstype<'a>(mountinfo: &'a Vec<procfs::process::MountInfo>, path: &Path) -> &'a str {
    &mountinfo
        .iter()
        .max_by_key(|info| {
            if path.starts_with(&info.mount_point) {
                info.mount_point.components().count() as i64
            } else {
                -1
            }
        })
        .unwrap()
        .fs_type
}
