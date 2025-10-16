// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::output_management::v1::server::zcosmic_output_configuration_v1;
use smithay::{
    output::{Mode, Output, WeakOutput},
    reexports::{
        wayland_protocols_wlr::output_management::v1::server::{
            zwlr_output_configuration_head_v1::{self, ZwlrOutputConfigurationHeadV1},
            zwlr_output_configuration_v1::{self, ZwlrOutputConfigurationV1},
            zwlr_output_head_v1::{self, ZwlrOutputHeadV1},
            zwlr_output_manager_v1::{self, ZwlrOutputManagerV1},
            zwlr_output_mode_v1::{self, ZwlrOutputModeV1},
        },
        wayland_server::{
            Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
            backend::ClientId,
        },
    },
    utils::{Point, Size},
};
use std::convert::TryInto;

use crate::wayland::protocols::output_configuration::*;

impl<D> GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData, D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZwlrOutputManagerV1>,
        _global_data: &OutputMngrGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let mut instance = OutputMngrInstance {
            obj: data_init.init(resource, ()),
            heads: Vec::new(),
        };

        let mngr_state = state.output_configuration_state();
        for output in &mngr_state.outputs {
            send_head_to_client::<D>(dh, &mut instance, output);
        }
        instance.obj.done(mngr_state.serial_counter);
        mngr_state.instances.push(instance);
    }

    fn can_view(client: Client, global_data: &OutputMngrGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZwlrOutputManagerV1, (), D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZwlrOutputManagerV1,
        request: zwlr_output_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_manager_v1::Request::CreateConfiguration { id, serial } => {
                let conf = data_init.init(
                    id,
                    PendingConfiguration::new(PendingConfigurationInner {
                        extension_obj: None,
                        serial,
                        used: false,
                        heads: Vec::new(),
                    }),
                );

                let state = state.output_configuration_state();
                if serial != state.serial_counter {
                    conf.cancelled();
                }
            }
            zwlr_output_manager_v1::Request::Stop => {
                let state = state.output_configuration_state();
                state.instances.retain(|instance| instance.obj != *obj);
                obj.finished();
            }
            _ => {}
        }
    }

    fn destroyed(
        state: &mut D,
        _client: wayland_backend::server::ClientId,
        obj: &ZwlrOutputManagerV1,
        _data: &(),
    ) {
        let state = state.output_configuration_state();
        state.instances.retain(|instance| instance.obj != *obj);
    }
}

impl<D> Dispatch<ZwlrOutputHeadV1, WeakOutput, D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZwlrOutputHeadV1,
        request: zwlr_output_head_v1::Request,
        _data: &WeakOutput,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        if let zwlr_output_head_v1::Request::Release = request {
            for instance in &mut state.output_configuration_state().instances {
                instance.heads.retain(|h| &h.obj != obj);
            }
        }
    }

    fn destroyed(state: &mut D, _client: ClientId, obj: &ZwlrOutputHeadV1, _data: &WeakOutput) {
        for instance in &mut state.output_configuration_state().instances {
            instance.heads.retain(|h| &h.obj != obj);
        }
    }
}

impl<D> Dispatch<ZwlrOutputModeV1, Mode, D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZwlrOutputModeV1,
        request: zwlr_output_mode_v1::Request,
        _data: &Mode,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        if let zwlr_output_mode_v1::Request::Release = request {
            let state = state.output_configuration_state();
            for instance in &mut state.instances {
                for head in &mut instance.heads {
                    head.modes.retain(|mode| mode != obj)
                }
            }
        }
    }
}

impl<D> Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration, D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZwlrOutputConfigurationV1,
        request: zwlr_output_configuration_v1::Request,
        data: &PendingConfiguration,
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_configuration_v1::Request::EnableHead { id, head } => {
                let mut pending = data.lock().unwrap();
                if pending.heads.iter().any(|(h, _)| *h == head) {
                    obj.post_error(
                        zwlr_output_configuration_v1::Error::AlreadyConfiguredHead,
                        format!("{:?} was already configured", head),
                    );
                    return;
                }

                let conf_head = data_init.init(id, PendingOutputConfiguration::default());
                pending.heads.push((head, Some(conf_head)));
            }
            zwlr_output_configuration_v1::Request::DisableHead { head } => {
                let mut pending = data.lock().unwrap();
                if pending.heads.iter().any(|(h, _)| *h == head) {
                    obj.post_error(
                        zwlr_output_configuration_v1::Error::AlreadyConfiguredHead,
                        format!("{:?} was already configured", head),
                    );
                    return;
                }

                if pending.heads.iter().any(|(_, c)| match c {
                    Some(conf) => {
                        let output_conf = conf.data::<PendingOutputConfiguration>().unwrap();
                        let output = head.data::<WeakOutput>().unwrap();
                        let pending_conf = output_conf.lock().unwrap();
                        pending_conf.mirroring.as_ref().is_some_and(|o| o == output)
                    }
                    None => false,
                }) {
                    if let Some(extension_obj) = pending.extension_obj.as_ref() {
                        extension_obj.post_error(
                            zcosmic_output_configuration_v1::Error::MirroredHeadBusy,
                            format!("{:?} is disabled and mirrored", head),
                        );
                    } else {
                        // unreachable?
                        head.post_error(
                            zwlr_output_configuration_v1::Error::UnconfiguredHead,
                            format!("{:?} is disabled and mirrored", head),
                        );
                    }
                }

                pending.heads.push((head, None));
            }
            x @ zwlr_output_configuration_v1::Request::Apply
            | x @ zwlr_output_configuration_v1::Request::Test => {
                let mut pending = data.lock().unwrap();

                if pending.used {
                    return obj.post_error(
                        zwlr_output_configuration_v1::Error::AlreadyUsed,
                        "Configuration object was used already".to_string(),
                    );
                }
                pending.used = true;

                let inner = state.output_configuration_state();
                if pending.serial != inner.serial_counter {
                    obj.cancelled();
                    if let Some(extension_obj) = pending.extension_obj.take() {
                        extension_obj.finished();
                    }
                    return;
                }

                let final_conf = match pending
                    .heads
                    .iter_mut()
                    .map(|(head, conf)| {
                        let output = match inner
                            .instances
                            .iter()
                            .find_map(|instance| instance.heads.iter().find(|h| h.obj == *head))
                            .map(|i| i.output.clone())
                        {
                            Some(o) => o,
                            None => {
                                return Err(zwlr_output_configuration_head_v1::Error::InvalidMode);
                            }
                        };

                        match conf {
                            Some(head) => (&mut *head
                                .data::<PendingOutputConfiguration>()
                                .unwrap()
                                .lock()
                                .unwrap())
                                .try_into()
                                .map(|c| (output, c)),
                            None => Ok((output, OutputConfiguration::Disabled)),
                        }
                    })
                    .collect::<Result<
                        Vec<(Output, OutputConfiguration)>,
                        zwlr_output_configuration_head_v1::Error,
                    >>() {
                    Ok(conf) => conf,
                    Err(code) => {
                        return obj.post_error(code, "Incomplete configuration".to_string());
                    }
                };

                let configured_outputs = final_conf
                    .iter()
                    .map(|(o, _)| o.clone())
                    .collect::<Vec<_>>();

                // handle potential races of destroyed heads and modes with cancel instead of a protocol error
                // 1. If len doesn't match some outputs aren't configured (technically a protocol error)
                // 2. If any configured output isn't in our list anymore, but we passed the len()-test,
                //      we raced a new head and a destroyed head, so again cancel.
                // 3. If the selected mode isn't in our list anymore, we probably already send `finished` for
                //    the mode, but got no release yet. So again, racy -> cancel.
                if configured_outputs.len() != inner.outputs.len()
                    || configured_outputs
                        .iter()
                        .any(|o| !inner.outputs.contains(o))
                    || final_conf.iter().any(|(o, c)| match c {
                        OutputConfiguration::Enabled {
                            mode: Some(ModeConfiguration::Mode(m)),
                            ..
                        } => !o.modes().contains(m),
                        _ => false,
                    })
                {
                    obj.cancelled();
                    if let Some(extension_obj) = pending.extension_obj.take() {
                        extension_obj.finished();
                    }
                    return;
                }

                let result = if matches!(x, zwlr_output_configuration_v1::Request::Test) {
                    state.test_configuration(final_conf)
                } else {
                    state.apply_configuration(final_conf)
                };

                if result {
                    obj.succeeded();
                } else {
                    obj.failed();
                }
                if let Some(extension_obj) = pending.extension_obj.take() {
                    extension_obj.finished();
                }
            }
            zwlr_output_configuration_v1::Request::Destroy => {
                let mut pending = data.lock().unwrap();
                if let Some(extension_obj) = pending.extension_obj.take() {
                    extension_obj.finished();
                }
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration, D>
    for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        obj: &ZwlrOutputConfigurationHeadV1,
        request: zwlr_output_configuration_head_v1::Request,
        data: &PendingOutputConfiguration,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_configuration_head_v1::Request::SetMode { mode } => {
                let mut pending = data.lock().unwrap();
                if pending.mode.is_some() {
                    obj.post_error(
                        zwlr_output_configuration_head_v1::Error::AlreadySet,
                        format!("{:?} already had a mode configured", obj),
                    );
                    return;
                }
                pending.mode = Some(ModeConfiguration::Mode(mode));
            }
            zwlr_output_configuration_head_v1::Request::SetCustomMode {
                width,
                height,
                refresh,
            } => {
                let mut pending = data.lock().unwrap();
                if pending.mode.is_some() {
                    obj.post_error(
                        zwlr_output_configuration_head_v1::Error::AlreadySet,
                        format!("{:?} already had a mode configured", obj),
                    );
                    return;
                }
                pending.mode = Some(ModeConfiguration::Custom {
                    size: Size::from((width, height)),
                    refresh: if refresh == 0 { None } else { Some(refresh) },
                });
            }
            zwlr_output_configuration_head_v1::Request::SetPosition { x, y } => {
                let mut pending = data.lock().unwrap();
                if pending.position.is_some() {
                    obj.post_error(
                        zwlr_output_configuration_head_v1::Error::AlreadySet,
                        format!("{:?} already had a position configured", obj),
                    );
                    return;
                }
                pending.position = Some(Point::from((x, y)));
            }
            zwlr_output_configuration_head_v1::Request::SetScale { scale } => {
                let mut pending = data.lock().unwrap();
                if pending.scale.is_some() {
                    obj.post_error(
                        zwlr_output_configuration_head_v1::Error::AlreadySet,
                        format!("{:?} already had a scale configured", obj),
                    );
                    return;
                }
                pending.scale = Some(scale);
            }
            zwlr_output_configuration_head_v1::Request::SetTransform { transform } => {
                let mut pending = data.lock().unwrap();
                if pending.transform.is_some() {
                    obj.post_error(
                        zwlr_output_configuration_head_v1::Error::AlreadySet,
                        format!("{:?} already had a transform configured", obj),
                    );
                    return;
                }
                pending.transform = Some(match transform.into_result() {
                    Ok(transform) => transform.into(),
                    Err(err) => {
                        obj.post_error(
                            zwlr_output_configuration_head_v1::Error::InvalidTransform,
                            format!("Invalid transform: {:?}", err),
                        );
                        return;
                    }
                });
            }
            zwlr_output_configuration_head_v1::Request::SetAdaptiveSync { state } => {
                let mut pending = data.lock().unwrap();
                if pending.adaptive_sync.is_some() {
                    obj.post_error(
                        zwlr_output_configuration_head_v1::Error::AlreadySet,
                        format!("{:?} already had adaptive sync configured", obj),
                    );
                    return;
                }
                pending.adaptive_sync = Some(match state.into_result() {
                    Ok(state) => match state {
                        zwlr_output_head_v1::AdaptiveSyncState::Enabled => AdaptiveSync::Force,
                        _ => AdaptiveSync::Disabled,
                    },
                    Err(err) => {
                        obj.post_error(
                            zwlr_output_configuration_head_v1::Error::InvalidAdaptiveSyncState,
                            format!("Invalid adaptive sync value: {:?}", err),
                        );
                        return;
                    }
                });
            }
            _ => {}
        }
    }
}
