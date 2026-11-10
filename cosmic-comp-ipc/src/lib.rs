use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    io,
    os::{
        fd::{BorrowedFd, FromRawFd, IntoRawFd, RawFd},
        unix::net::UnixStream,
    },
};

#[cfg(feature = "async")]
mod nonblocking;
#[cfg(feature = "async")]
pub use self::nonblocking::*;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "message")]
pub enum Message {
    SetEnv { variables: HashMap<String, String> },
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to serialize message")]
    Serialize(#[source] serde_json::Error),
    #[error("Failed to deserialize message")]
    Deserialize(#[source] serde_json::Error),
    #[error(transparent)]
    IO(#[from] io::Error),
}

unsafe fn set_cloexec(fd: RawFd, set: bool) -> rustix::io::Result<()> {
    if fd == -1 {
        return Err(rustix::io::Errno::BADF);
    }
    let fd = unsafe { BorrowedFd::borrow_raw(fd) };
    let flags = rustix::io::fcntl_getfd(fd)?;
    if set {
        rustix::io::fcntl_setfd(fd, flags | rustix::io::FdFlags::CLOEXEC)
    } else {
        rustix::io::fcntl_setfd(fd, flags & rustix::io::FdFlags::CLOEXEC.complement())
    }
}

/// Searches for a passed cosmic session socket and returns a [`UnixStream`] if found.
///
/// *Note:* If you need an async variant of the socket, use e.g.
/// - [`async_io::Async::new`](https://docs.rs/async-io/latest/async_io/struct.Async.html#method.new)
/// - [`tokio::net::UnixStream::from_std`](https://docs.rs/tokio/latest/tokio/net/struct.UnixStream.html#method.from_std)
pub fn find_ipc_socket() -> Result<Option<UnixStream>, Error> {
    if let Ok(fd_num) = std::env::var("COSMIC_SESSION_SOCK")
        && let Ok(fd) = fd_num.parse::<RawFd>()
    {
        let res = unsafe { set_cloexec(fd, true) };
        if res.is_err() {
            unsafe { rustix::io::close(fd) };
        }
        res.map(|()| Some(unsafe { UnixStream::from_raw_fd(fd) }))
            .map_err(|err| Error::IO(err.kind().into()))
    } else {
        Ok(None)
    }
}

/// Creates a socket pair and returns one end as well as a map of env variables
/// to pass to a spawned cosmic-comp instance.
///
/// *Note:* If you need an async variant of the socket, use e.g.
/// - [`async_io::Async::new`](https://docs.rs/async-io/latest/async_io/struct.Async.html#method.new)
/// - [`tokio::net::UnixStream::from_std`](https://docs.rs/tokio/latest/tokio/net/struct.UnixStream.html#method.from_std)
pub fn setup_socket() -> Result<(UnixStream, HashMap<String, String>), Error> {
    let (session, comp) = UnixStream::pair()?;
    let comp_fd = comp.into_raw_fd();
    unsafe { set_cloexec(comp_fd, false) }.map_err(|err| Error::IO(err.kind().into()))?;
    let mut env_map = HashMap::new();
    env_map.insert("COSMIC_SESSION_SOCK".to_string(), comp_fd.to_string());
    Ok((session, env_map))
}

/// Encode and write a cosmic-comp ipc message to a given `std::io` compatible writer.
///
/// **Note:** This method does not deal with buffering partial message,
/// as it is meant to be used with unix sockets with provide message
/// atomicity.
pub fn write_msg_blocking<W: io::Write>(writer: &mut W, msg: &Message) -> Result<(), Error> {
    let message = serde_json::to_string(msg).map_err(Error::Serialize)?;
    let bytes = message.into_bytes();
    let len = (bytes.len() as u16).to_ne_bytes();
    writer.write_all(&len)?;
    writer.write_all(&bytes)?;
    writer.flush()?;
    Ok(())
}

/// Read and parse a cosmic-comp ipc message from a given `std::io` compatible reader.
///
/// **Note:** This method does not deal with buffering partial message,
/// as it is meant to be used with unix sockets with provide message
/// atomicity.
pub fn read_msg_blocking<R: io::Read>(reader: &mut R) -> Result<Message, Error> {
    let mut buf = [0u8; 2];
    reader.read_exact(&mut buf)?;
    let len = u16::from_ne_bytes(buf);

    let mut msg_buf = vec![0u8; len as usize];
    reader.read_exact(&mut msg_buf)?;
    serde_json::from_slice(&msg_buf).map_err(Error::Deserialize)
}
