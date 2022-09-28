// SPDX-License-Identifier: GPL-3.0-only

use std::{
    fmt,
    os::unix::io::{AsFd, AsRawFd, BorrowedFd, OwnedFd, RawFd},
    sync::Arc,
};

#[derive(Clone)]
pub struct SessionFd(Arc<OwnedFd>);

impl SessionFd {
    pub fn new(fd: OwnedFd) -> SessionFd {
        SessionFd(Arc::new(fd))
    }
}

impl fmt::Debug for SessionFd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Session-provided File Descriptor [{}]",
            self.0.as_raw_fd()
        )
    }
}

impl AsFd for SessionFd {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.0.as_fd()
    }
}

impl AsRawFd for SessionFd {
    fn as_raw_fd(&self) -> RawFd {
        self.0.as_raw_fd()
    }
}
