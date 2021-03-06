// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::nix::unistd::close;
use std::{
    fmt,
    os::unix::io::{AsRawFd, RawFd},
    sync::Arc,
};

#[derive(Clone)]
pub struct SessionFd(Arc<DropFd>);

struct DropFd(RawFd);

impl SessionFd {
    pub fn new(fd: RawFd) -> SessionFd {
        SessionFd(Arc::new(DropFd(fd)))
    }
}

impl fmt::Debug for SessionFd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Session-provided File Descriptor [{}]", self.0 .0)
    }
}

impl AsRawFd for SessionFd {
    fn as_raw_fd(&self) -> RawFd {
        self.0 .0
    }
}

impl Drop for DropFd {
    fn drop(&mut self) {
        if let Err(err) = close(self.0) {
            slog_scope::warn!("Failed to close file descriptor {}: {}", self.0, err);
        }
    }
}
