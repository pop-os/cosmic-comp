// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    output::{Mode, Output},
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
        state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZcosmicOutputManagerV1>,
        _global_data: &OutputMngrGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let obj = data_init.init(resource, ());
        let mngr_state = state.output_configuration_state();
        mngr_state.extension_instances.push(obj);
    }

    fn can_view(client: Client, global_data: &OutputMngrGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ZcosmicOutputManagerV1, (), D> for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, OutputMngrInstanceData>
        + Dispatch<ZwlrOutputHeadV1, Output>
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
                if let Some(pending) = config.data::<PendingConfiguration>() {
                    let obj = data_init.init(extended, config.downgrade());
                    pending.lock().unwrap().extension_obj = Some(obj);
                }
            }
            zcosmic_output_manager_v1::Request::GetConfigurationHead {
                extended,
                config_head,
            } => {
                if let Some(pending) = config_head.data::<PendingOutputConfiguration>() {
                    let obj = data_init.init(extended, config_head.downgrade());
                    pending.lock().unwrap().extension_obj = Some(obj);
                }
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZcosmicOutputHeadV1, Weak<ZwlrOutputHeadV1>, D> for OutputConfigurationState<D> {
    fn request(
        _state: &mut D,
        _client: &Client,
        _obj: &ZcosmicOutputHeadV1,
        _request: zcosmic_output_head_v1::Request,
        _data: &Weak<ZwlrOutputHeadV1>,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
    }
}

impl<D> Dispatch<ZcosmicOutputConfigurationV1, Weak<ZwlrOutputConfigurationV1>, D>
    for OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, OutputMngrInstanceData>
        + Dispatch<ZwlrOutputHeadV1, Output>
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
                    if let Some(data) = obj.data::<PendingConfiguration>() {
                        if let Some(output) = mirroring.data::<Output>() {
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
                                        if let Some(conf) = c.data::<PendingOutputConfiguration>() {
                                            match conf.lock().unwrap().mirroring.as_ref() {
                                                Some(mirrored) => {
                                                    head.data::<Output>().is_some_and(|o| o == mirrored) // we are already a mirror target -> invalid
                                                    || *h == mirroring // our target already mirrors -> invalid
                                                }
                                                None => false,
                                            }
                                        } else {
                                            *h == mirroring // unknown state for our mirror target -> invalid
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
                        }
                    }
                }
            }
            zcosmic_output_configuration_v1::Request::Destroy => {
                if let Ok(obj) = obj.upgrade() {
                    if let Some(data) = obj.data::<PendingConfiguration>() {
                        let mut pending = data.lock().unwrap();
                        let _ = pending.extension_obj.take();
                        pending.heads.retain(|(_, conf)| match conf {
                            Some(head) => {
                                if let Some(data) = head.data::<PendingOutputConfiguration>() {
                                    let output_conf = data.lock().unwrap();
                                    output_conf.mirroring.is_none()
                                } else {
                                    true
                                }
                            }
                            None => true,
                        })
                    }
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
        + Dispatch<ZwlrOutputManagerV1, OutputMngrInstanceData>
        + Dispatch<ZwlrOutputHeadV1, Output>
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
                    if let Some(data) = obj.data::<PendingOutputConfiguration>() {
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
            }
            _ => {}
        }
    }
}
