// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    output::Mode,
    reexports::{
        wayland_protocols_wlr::output_management::v1::server::{
            zwlr_output_configuration_head_v1::{self, ZwlrOutputConfigurationHeadV1},
            zwlr_output_configuration_v1::{self, ZwlrOutputConfigurationV1},
            zwlr_output_head_v1::ZwlrOutputHeadV1,
        },
        wayland_server::{
            Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        },
    },
};

use cosmic_comp_config::output::comp::OutputState as EnabledState;

use cosmic_protocols::output_management::v1::server::{
    zcosmic_output_configuration_head_v1::{self, ZcosmicOutputConfigurationHeadV1},
    zcosmic_output_configuration_v1::{self, ZcosmicOutputConfigurationV1},
    zcosmic_output_head_v1::{self, ZcosmicOutputHeadV1},
    zcosmic_output_manager_v1::{self, ZcosmicOutputManagerV1},
};

use crate::wayland::protocols::output_configuration::*;

impl<D> GlobalDispatch<ZcosmicOutputManagerV1, OutputMngrGlobalData, D>
    for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZcosmicOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZcosmicOutputManagerV1, ()>
        + Dispatch<ZcosmicOutputHeadV1, Weak<ZwlrOutputHeadV1>>
        + Dispatch<ZcosmicOutputConfigurationV1, Weak<ZwlrOutputConfigurationV1>>
        + Dispatch<ZcosmicOutputConfigurationHeadV1, Weak<ZwlrOutputConfigurationHeadV1>>
        + OutputConfigurationHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicOutputManagerV1>,
        _global_data: &OutputMngrGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }

    fn can_view(client: Client, global_data: &OutputMngrGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicOutputManagerV1, (), D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + GlobalDispatch<ZcosmicOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZcosmicOutputManagerV1, ()>
        + Dispatch<ZcosmicOutputHeadV1, Weak<ZwlrOutputHeadV1>>
        + Dispatch<ZcosmicOutputConfigurationV1, Weak<ZwlrOutputConfigurationV1>>
        + Dispatch<ZcosmicOutputConfigurationHeadV1, Weak<ZwlrOutputConfigurationHeadV1>>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _obj: &ZcosmicOutputManagerV1,
        request: zcosmic_output_manager_v1::Request,
        _data: &(),
        dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_output_manager_v1::Request::GetHead { extended, head } => {
                let inner = state.output_configuration_state();
                if let Some(mngr) = inner
                    .instances
                    .iter_mut()
                    .find(|instance| instance.heads.iter().any(|instance| instance.obj == head))
                {
                    let head_data = mngr
                        .heads
                        .iter_mut()
                        .find(|instance| instance.obj == head)
                        .unwrap();
                    let obj = data_init.init(extended, head.downgrade());
                    head_data.extension_obj = Some(obj);
                    let output = head_data.output.clone();

                    send_head_to_client::<D>(dh, mngr, &output);
                    for manager in inner.instances.iter() {
                        manager.obj.done(inner.serial_counter);
                    }
                }
            }
            zcosmic_output_manager_v1::Request::GetConfiguration { extended, config } => {
                let pending = config.data::<PendingConfiguration>().unwrap();
                let obj = data_init.init(extended, config.downgrade());
                pending.lock().unwrap().extension_obj = Some(obj);
            }
            zcosmic_output_manager_v1::Request::GetConfigurationHead {
                extended,
                config_head,
            } => {
                data_init.init(extended, config_head.downgrade());
            }
            zcosmic_output_manager_v1::Request::SetXwaylandPrimary { head } => {
                let Some(head) = head else {
                    state.request_xwayland_primary(None);
                    return;
                };

                let inner = state.output_configuration_state();
                if let Some(head_data) = inner.instances.iter_mut().find_map(|instance| {
                    instance
                        .heads
                        .iter()
                        .find(|instance| instance.extension_obj.as_ref() == Some(&head))
                }) {
                    let output = head_data.output.clone();
                    if output.config().enabled == EnabledState::Enabled {
                        state.request_xwayland_primary(Some(output));
                    }
                }
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZcosmicOutputHeadV1, Weak<ZwlrOutputHeadV1>, D> for OutputConfigurationState<D>
where
    D: OutputConfigurationHandler + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        obj: &ZcosmicOutputHeadV1,
        request: zcosmic_output_head_v1::Request,
        _data: &Weak<ZwlrOutputHeadV1>,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_output_head_v1::Request::Release => {
                let inner = state.output_configuration_state();
                if let Some(head) = inner
                    .instances
                    .iter_mut()
                    .flat_map(|instance| instance.heads.iter_mut())
                    .find(|head| head.extension_obj.as_ref().is_some_and(|o| o == obj))
                {
                    head.extension_obj.take();
                }
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZcosmicOutputConfigurationV1, Weak<ZwlrOutputConfigurationV1>, D>
    for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + GlobalDispatch<ZcosmicOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZcosmicOutputManagerV1, ()>
        + Dispatch<ZcosmicOutputHeadV1, Weak<ZwlrOutputHeadV1>>
        + Dispatch<ZcosmicOutputConfigurationV1, Weak<ZwlrOutputConfigurationV1>>
        + Dispatch<ZcosmicOutputConfigurationHeadV1, Weak<ZwlrOutputConfigurationHeadV1>>
        + OutputConfigurationHandler
        + 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        extension_obj: &ZcosmicOutputConfigurationV1,
        request: zcosmic_output_configuration_v1::Request,
        obj: &Weak<ZwlrOutputConfigurationV1>,
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_output_configuration_v1::Request::MirrorHead {
                id,
                head,
                mirroring,
            } => {
                if let Ok(obj) = obj.upgrade() {
                    let data = obj.data::<PendingConfiguration>().unwrap();
                    if let Some(output) = mirroring.data::<WeakOutput>().unwrap().upgrade() {
                        let mut pending = data.lock().unwrap();
                        if pending.heads.iter().any(|(h, _)| *h == head) {
                            obj.post_error(
                                zwlr_output_configuration_v1::Error::AlreadyConfiguredHead,
                                format!("{:?} was already configured", head),
                            );
                            return;
                        }

                        if pending.heads.iter().any(|(h, c)| {
                            match c.as_ref() {
                                Some(c) => {
                                    let conf = c.data::<PendingOutputConfiguration>().unwrap();
                                    match conf.lock().unwrap().mirroring.as_ref() {
                                        Some(mirrored) => {
                                            head.data::<WeakOutput>().unwrap() == mirrored // we are already a mirror target -> invalid
                                            || *h == mirroring // our target already mirrors -> invalid
                                        }
                                        None => false,
                                    }
                                }
                                None => *h == mirroring, // disabled state for our mirror target -> invalid
                            }
                        }) {
                            extension_obj.post_error(
                            zcosmic_output_configuration_v1::Error::MirroredHeadBusy,
                            format!("{:?} can't mirror, it is either a mirror target itself or {:?} is not enabled/already mirroring", head, mirroring),
                        );
                        }

                        let output_conf = PendingOutputConfiguration::default();
                        output_conf.lock().unwrap().mirroring = Some(output.clone());
                        let conf_head = data_init.init(id, output_conf);
                        pending.heads.push((head, Some(conf_head)));
                    } else {
                        let output_conf = PendingOutputConfiguration::default();
                        data_init.init(id, output_conf);
                    }
                } else {
                    let output_conf = PendingOutputConfiguration::default();
                    data_init.init(id, output_conf);
                }
            }
            zcosmic_output_configuration_v1::Request::Release => {
                if let Ok(obj) = obj.upgrade() {
                    let data = obj.data::<PendingConfiguration>().unwrap();
                    data.lock().unwrap().extension_obj.take();
                }
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZcosmicOutputConfigurationHeadV1, Weak<ZwlrOutputConfigurationHeadV1>, D>
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
        _extended_obj: &ZcosmicOutputConfigurationHeadV1,
        request: zcosmic_output_configuration_head_v1::Request,
        obj: &Weak<ZwlrOutputConfigurationHeadV1>,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zcosmic_output_configuration_head_v1::Request::SetScale1000 { scale_1000 } => {
                if let Ok(obj) = obj.upgrade() {
                    let data = obj.data::<PendingOutputConfiguration>().unwrap();
                    let mut pending = data.lock().unwrap();
                    if pending.scale.is_some() {
                        obj.post_error(
                            zwlr_output_configuration_head_v1::Error::AlreadySet,
                            format!("{:?} already had a scale configured", obj),
                        );
                        return;
                    }
                    pending.scale = Some((scale_1000 as f64) / 1000.0);
                }
            }
            zcosmic_output_configuration_head_v1::Request::SetAdaptiveSyncExt { state } => {
                if let Ok(obj) = obj.upgrade() {
                    let data = obj.data::<PendingOutputConfiguration>().unwrap();
                    let mut pending = data.lock().unwrap();
                    if pending.adaptive_sync.is_some() {
                        obj.post_error(
                            zwlr_output_configuration_head_v1::Error::AlreadySet,
                            format!("{:?} already had an adaptive_sync state configured", obj),
                        );
                        return;
                    }
                    pending.adaptive_sync = match state.into_result() {
                        Ok(zcosmic_output_head_v1::AdaptiveSyncStateExt::Always) => {
                            Some(AdaptiveSync::Force)
                        }
                        Ok(zcosmic_output_head_v1::AdaptiveSyncStateExt::Automatic) => {
                            Some(AdaptiveSync::Enabled)
                        }
                        _ => Some(AdaptiveSync::Disabled),
                    };
                }
            }
            _ => {}
        }
    }
}
