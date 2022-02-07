use libarchive3_sys::ffi::Struct_archive_entry;
use std::os::raw::{c_char, c_int};

///// FFI
const ARCHIVE_ENTRY_DIGEST_SHA256: c_int = 0x00000004;

#[link(name = "archive")]
extern "C" {
    fn archive_entry_digest(arg1: *mut Struct_archive_entry, arg2: c_int) -> *const c_char;
}

pub fn get_sha256<'a>(entry: &'a impl libarchive::archive::Entry) -> Option<&'a [u8; 32]> {
    unsafe {
        let ptr = archive_entry_digest(entry.entry(), ARCHIVE_ENTRY_DIGEST_SHA256) as *const u8;
        if ptr.is_null() {
            return None;
        }
        Some(&*(ptr as *const [u8; 32]))
    }
}
