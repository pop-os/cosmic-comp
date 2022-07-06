// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::{
    calloop::{generic::Generic, Interest, LoopHandle, Mode, PostAction},
    nix::{fcntl, unistd},
};

use anyhow::{anyhow, Context, Result};
use sendfd::RecvWithFd;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    io::{Read, Write},
    os::unix::{
        io::{AsRawFd, FromRawFd, RawFd},
        net::UnixStream,
    },
    sync::Arc,
};

use crate::state::{Data, State};

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
impl AsRawFd for StreamWrapper {
    fn as_raw_fd(&self) -> RawFd {
        self.stream.as_raw_fd()
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

pub fn setup_socket(handle: LoopHandle<Data>, state: &State) -> Result<()> {
    if let Ok(fd_num) = std::env::var("COSMIC_SESSION_SOCK") {
        if let Ok(fd) = fd_num.parse::<RawFd>() {
            // set CLOEXEC
            let flags = fcntl::fcntl(fd, fcntl::FcntlArg::F_GETFD);
            let result = flags
                .map(|f| fcntl::FdFlag::from_bits(f).unwrap() | fcntl::FdFlag::FD_CLOEXEC)
                .and_then(|f| fcntl::fcntl(fd, fcntl::FcntlArg::F_SETFD(f)));
            let mut session_socket = match result {
                // CLOEXEC worked and we can startup with session IPC
                Ok(_) => unsafe { UnixStream::from_raw_fd(fd) },
                // CLOEXEC didn't work, something is wrong with the fd, just close it
                Err(err) => {
                    let _ = unistd::close(fd);
                    return Err(err).with_context(|| "Failed to setup session socket");
                }
            };

            let mut env = HashMap::new();
            env.insert(
                String::from("WAYLAND_DISPLAY"),
                state
                    .common
                    .socket
                    .clone()
                    .into_string()
                    .map_err(|_| anyhow!("wayland socket is no valid utf-8 string?"))?,
            );
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
                move |_, stream, data: &mut crate::state::Data| {
                    if stream.size == 0 {
                        let mut len = [0u8; 2];
                        match stream.stream.read_exact(&mut len) {
                            Ok(()) => {
                                stream.size = u16::from_ne_bytes(len);
                                stream.buffer = vec![0; stream.size as usize];
                            },
                            Err(err) => {
                                slog_scope::warn!("Error reading from session socket: {}", err);
                                return Ok(PostAction::Remove);
                            }
                        }
                    }

                    stream.read_bytes += match stream.stream.read(&mut stream.buffer) {
                        Ok(size) => size,
                        Err(err) => {
                            slog_scope::error!("Error reading from session socket: {}", err);
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
                                                    let stream = unsafe { UnixStream::from_raw_fd(fd) };
                                                    if let Err(err) = data.display.handle().insert_client(stream, Arc::new(data.state.new_privileged_client_state())) {
                                                        slog_scope::warn!("Failed to add privileged client to display: {}", err);
                                                    }
                                                }
                                            },
                                            Err(err) => {
                                                slog_scope::warn!("Failed to read file descriptors from session sock: {}", err);
                                            }
                                        }
                                    },
                                    Ok(Message::SetEnv { .. }) => slog_scope::warn!("Got SetEnv from session? What is this?"),
                                    _ => slog_scope::warn!("Unknown session socket message, are you using incompatible cosmic-session and cosmic-comp versions?"),
                                };
                                Ok(PostAction::Continue)
                            },
                            Err(err) => {
                                slog_scope::warn!("Invalid message from session sock: {}", err);
                                Ok(PostAction::Continue)
                            }
                        }
                    } else {
                        Ok(PostAction::Continue)
                    }
                },
            ).with_context(|| "Failed to init the cosmic session socket source")?;
        }
        slog_scope::error!("COSMIC_SESSION_SOCK is no valid RawFd: {}", fd_num);
    };

    Ok(())
}
