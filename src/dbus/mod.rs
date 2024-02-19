use crate::state::{BackendData, State};
use anyhow::{Context, Result};
use calloop::{InsertError, LoopHandle, RegistrationToken};

mod power;

pub fn init(evlh: &LoopHandle<'static, State>) -> Result<Vec<RegistrationToken>> {
    let mut tokens = Vec::new();

    match power::init() {
        Ok(power_daemon) => {
            let (tx, rx) = calloop::channel::channel();

            let token = evlh
                .insert_source(rx, |event, _, state| match event {
                    calloop::channel::Event::Msg(_) => {
                        let nodes = match &mut state.backend {
                            BackendData::Kms(kms) => {
                                kms.devices.keys().cloned().collect::<Vec<_>>()
                            }
                            _ => Vec::new(),
                        };
                        for node in nodes {
                            if let Err(err) = state.device_changed(node.dev_id()) {
                                tracing::error!(?err, "Failed to update drm device {}.", node);
                            }
                        }
                        ()
                    }
                    calloop::channel::Event::Closed => (),
                })
                .map_err(|InsertError { error, .. }| error)
                .with_context(|| "Failed to add channel to event_loop")?;

            // start helper thread
            let result = std::thread::Builder::new()
                .name("system76-power-hotplug".to_string())
                .spawn(move || {
                    if let Ok(mut msg_iter) = power_daemon.receive_hot_plug_detect() {
                        while let Some(msg) = msg_iter.next() {
                            if tx.send(msg).is_err() {
                                break;
                            }
                        }
                    }
                })
                .with_context(|| "Failed to start helper thread");

            match result {
                Ok(_handle) => {
                    tokens.push(token);
                    // detach thread
                }
                Err(err) => {
                    evlh.remove(token);
                    return Err(err);
                }
            }
        }
        Err(err) => {
            tracing::info!(?err, "Failed to connect to com.system76.PowerDaemon");
        }
    };

    Ok(tokens)
}
