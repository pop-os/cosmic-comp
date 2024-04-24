// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::{
    calloop::{generic::Generic, Interest, LoopHandle, Mode, PostAction},
    rustix,
};

use anyhow::{anyhow, Context, Result};
use sendfd::RecvWithFd;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    io::{Read, Write},
    os::unix::{
        io::{AsFd, BorrowedFd, FromRawFd, RawFd},
        net::UnixStream,
    },
    sync::Arc,
};
use tracing::{error, warn};

use crate::state::{ClientState, Common, State};

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "message")]
pub enum Message {
    SetEnv { variables: HashMap<String, String> },
    NewPrivilegedClient { count: usize },
}

struct StreamWrapper {
    stream: UnixStream,
    buffer: Vec<u8>,
    size: u16,
    read_bytes: usize,
}
impl AsFd for StreamWrapper {
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.stream.as_fd()
    }
}
impl From<UnixStream> for StreamWrapper {
    fn from(stream: UnixStream) -> StreamWrapper {
        StreamWrapper {
            stream,
            buffer: Vec::new(),
            size: 0,
            read_bytes: 0,
        }
    }
}

unsafe fn set_cloexec(fd: RawFd) -> rustix::io::Result<()> {
    if fd == -1 {
        return Err(rustix::io::Errno::BADF);
    }
    let fd = BorrowedFd::borrow_raw(fd);
    let flags = rustix::io::fcntl_getfd(fd)?;
    rustix::io::fcntl_setfd(fd, flags | rustix::io::FdFlags::CLOEXEC)
}

pub fn get_env(common: &Common) -> Result<HashMap<String, String>> {
    let mut env = HashMap::new();
    env.insert(
        String::from("WAYLAND_DISPLAY"),
        common
            .socket
            .clone()
            .into_string()
            .map_err(|_| anyhow!("wayland socket is no valid utf-8 string?"))?,
    );
    if let Some(display) = common.xwayland_state.as_ref().map(|s| s.display) {
        env.insert(String::from("DISPLAY"), format!(":{}", display));
    }
    Ok(env)
}

pub fn setup_socket(handle: LoopHandle<State>, common: &Common) -> Result<()> {
    if let Ok(fd_num) = std::env::var("COSMIC_SESSION_SOCK") {
        if let Ok(fd) = fd_num.parse::<RawFd>() {
            let mut session_socket = match unsafe { set_cloexec(fd) } {
                // CLOEXEC worked and we can startup with session IPC
                Ok(_) => unsafe { UnixStream::from_raw_fd(fd) },
                // CLOEXEC didn't work, something is wrong with the fd, just close it
                Err(err) => {
                    unsafe { rustix::io::close(fd) };
                    return Err(err).with_context(|| "Failed to setup session socket");
                }
            };

            let env = get_env(common)?;
            let message = serde_json::to_string(&Message::SetEnv { variables: env })
                .with_context(|| "Failed to encode environment variables into json")?;
            let bytes = message.into_bytes();
            let len = (bytes.len() as u16).to_ne_bytes();
            session_socket
                .write_all(&len)
                .with_context(|| "Failed to write message len")?;
            session_socket
                .write_all(&bytes)
                .with_context(|| "Failed to write message bytes")?;

            handle.insert_source(
                Generic::new(StreamWrapper::from(session_socket), Interest::READ, Mode::Level),
                move |_, stream, state| {
                    // SAFETY: We don't drop the stream!
                    let stream = unsafe { stream.get_mut() };

                    if stream.size == 0 {
                        let mut len = [0u8; 2];
                        match stream.stream.read_exact(&mut len) {
                            Ok(()) => {
                                stream.size = u16::from_ne_bytes(len);
                                stream.buffer = vec![0; stream.size as usize];
                            },
                            Err(err) => {
                                warn!(?err, "Error reading from session socket");
                                return Ok(PostAction::Remove);
                            }
                        }
                    }

                    stream.read_bytes += match stream.stream.read(&mut stream.buffer) {
                        Ok(size) => size,
                        Err(err) => {
                            error!(?err, "Error reading from session socket");
                            return Ok(PostAction::Remove);
                        }
                    };

                    if stream.read_bytes != 0 && stream.read_bytes == stream.size as usize {
                        stream.size = 0;
                        stream.read_bytes = 0;
                        match std::str::from_utf8(&stream.buffer) {
                            Ok(message) => {
                                match serde_json::from_str::<'_, Message>(&message) {
                                    Ok(Message::NewPrivilegedClient { count }) => {
                                        let mut buffer = [0; 1];
                                        let mut fds = vec![0; count];
                                        match stream.stream.recv_with_fd(&mut buffer, &mut *fds) {
                                            Ok((_, received_count)) => {
                                                assert_eq!(received_count, count);
                                                for fd in fds.into_iter().take(received_count) {
                                                    if fd == -1 {
                                                        continue;
                                                    }
                                                    let stream = unsafe { UnixStream::from_raw_fd(fd) };
                                                    let client_state = Arc::new(ClientState {
                                                        privileged: true,
                                                        ..state.new_client_state()
                                                    });
                                                    if let Err(err) = state.common.display_handle.insert_client(stream, client_state) {
                                                        warn!(?err, "Failed to add privileged client to display");
                                                    }
                                                }
                                            },
                                            Err(err) => {
                                                warn!(?err, "Failed to read file descriptors from session sock");
                                            }
                                        }
                                    },
                                    Ok(Message::SetEnv { .. }) => warn!("Got SetEnv from session? What is this?"),
                                    _ => warn!("Unknown session socket message, are you using incompatible cosmic-session and cosmic-comp versions?"),
                                };
                                Ok(PostAction::Continue)
                            },
                            Err(err) => {
                                warn!(?err, "Invalid message from session sock");
                                Ok(PostAction::Continue)
                            }
                        }
                    } else {
                        Ok(PostAction::Continue)
                    }
                },
            ).with_context(|| "Failed to init the cosmic session socket source")?;
        } else {
            error!(socket = fd_num, "COSMIC_SESSION_SOCK is no valid RawFd.");
        }
    };

    Ok(())
}
