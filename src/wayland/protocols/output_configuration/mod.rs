// SPDX-License-Identifier: GPL-3.0-only

use calloop::{
    timer::{TimeoutAction, Timer},
    LoopHandle,
};
use cosmic_comp_config::output::comp::AdaptiveSync;
use cosmic_protocols::output_management::v1::server::{
    zcosmic_output_configuration_head_v1::ZcosmicOutputConfigurationHeadV1,
    zcosmic_output_configuration_v1::ZcosmicOutputConfigurationV1,
    zcosmic_output_head_v1::{self, ZcosmicOutputHeadV1},
    zcosmic_output_manager_v1::ZcosmicOutputManagerV1,
};
use smithay::{
    backend::drm::VrrSupport,
    output::{Mode, Output, WeakOutput},
    reexports::{
        wayland_protocols_wlr::output_management::v1::server::{
            zwlr_output_configuration_head_v1::{self, ZwlrOutputConfigurationHeadV1},
            zwlr_output_configuration_v1::ZwlrOutputConfigurationV1,
            zwlr_output_head_v1::{self, ZwlrOutputHeadV1},
            zwlr_output_manager_v1::ZwlrOutputManagerV1,
            zwlr_output_mode_v1::ZwlrOutputModeV1,
        },
        wayland_server::{
            backend::GlobalId, protocol::wl_output::WlOutput, Client, Dispatch, DisplayHandle,
            GlobalDispatch, Resource, Weak,
        },
    },
    utils::{Logical, Physical, Point, Size, Transform},
    wayland::output::WlOutputData,
};
use std::{convert::TryFrom, sync::Mutex, time::Duration};

mod handlers;

pub fn head_is_enabled(output: &Output) -> bool {
    output
        .user_data()
        .get::<OutputState>()
        .map_or(false, |inner| inner.lock().unwrap().enabled)
}

#[derive(Debug)]
pub struct OutputConfigurationState<D> {
    outputs: Vec<Output>,
    removed_outputs: Vec<Output>,
    instances: Vec<OutputMngrInstance>,
    serial_counter: u32,
    global: GlobalId,
    extension_global: GlobalId,
    dh: DisplayHandle,
    event_loop_handle: LoopHandle<'static, D>,
    _dispatch: std::marker::PhantomData<D>,
}

pub trait OutputConfigurationHandler: Sized {
    fn output_configuration_state(&mut self) -> &mut OutputConfigurationState<Self>;

    fn test_configuration(&mut self, conf: Vec<(Output, OutputConfiguration)>) -> bool;
    fn apply_configuration(&mut self, conf: Vec<(Output, OutputConfiguration)>) -> bool;

    fn request_xwayland_primary(&mut self, output: Option<Output>);
}

pub struct OutputMngrGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug)]
struct OutputMngrInstance {
    obj: ZwlrOutputManagerV1,
    heads: Vec<OutputHeadInstance>,
}

#[derive(Debug)]
struct OutputHeadInstance {
    obj: ZwlrOutputHeadV1,
    extension_obj: Option<ZcosmicOutputHeadV1>,
    output: Output,
    modes: Vec<ZwlrOutputModeV1>,
    finished: bool,
}

#[derive(Debug, Default)]
pub struct PendingConfigurationInner {
    extension_obj: Option<ZcosmicOutputConfigurationV1>,
    serial: u32,
    used: bool,
    heads: Vec<(ZwlrOutputHeadV1, Option<ZwlrOutputConfigurationHeadV1>)>,
}

pub type PendingConfiguration = Mutex<PendingConfigurationInner>;

#[derive(Debug, Clone)]
pub enum ModeConfiguration<M: Clone> {
    Mode(M),
    Custom {
        size: Size<i32, Physical>,
        refresh: Option<i32>,
    },
}

#[derive(Debug, Default, Clone)]
pub struct PendingOutputConfigurationInner {
    mirroring: Option<Output>,
    mode: Option<ModeConfiguration<ZwlrOutputModeV1>>,
    position: Option<Point<i32, Logical>>,
    transform: Option<Transform>,
    scale: Option<f64>,
    adaptive_sync: Option<AdaptiveSync>,
}
pub type PendingOutputConfiguration = Mutex<PendingOutputConfigurationInner>;

#[derive(Debug, Clone)]
pub enum OutputConfiguration {
    Enabled {
        mirroring: Option<Output>,
        mode: Option<ModeConfiguration<Mode>>,
        position: Option<Point<i32, Logical>>,
        transform: Option<Transform>,
        scale: Option<f64>,
        adaptive_sync: Option<AdaptiveSync>,
    },
    Disabled,
}

impl<'a> TryFrom<&'a mut PendingOutputConfigurationInner> for OutputConfiguration {
    type Error = zwlr_output_configuration_head_v1::Error;
    fn try_from(
        pending: &'a mut PendingOutputConfigurationInner,
    ) -> Result<OutputConfiguration, Self::Error> {
        let mode = match pending.mode.clone() {
            Some(ModeConfiguration::Mode(wlr_mode)) => Some(ModeConfiguration::Mode(
                wlr_mode
                    .data::<Mode>()
                    .cloned()
                    .ok_or_else(|| zwlr_output_configuration_head_v1::Error::InvalidMode)?,
            )),
            Some(ModeConfiguration::Custom { size, refresh }) => {
                Some(ModeConfiguration::Custom { size, refresh })
            }
            None => None,
        };
        Ok(OutputConfiguration::Enabled {
            mode,
            mirroring: pending.mirroring.clone(),
            position: pending.position,
            transform: pending.transform,
            scale: pending.scale,
            adaptive_sync: pending.adaptive_sync,
        })
    }
}

struct OutputStateInner {
    enabled: bool,
    global: Option<GlobalId>,
}
type OutputState = Mutex<OutputStateInner>;

impl<D> OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + GlobalDispatch<WlOutput, WlOutputData>
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
    pub fn new<F>(
        dh: &DisplayHandle,
        event_loop_handle: LoopHandle<'static, D>,
        client_filter: F,
    ) -> OutputConfigurationState<D>
    where
        F: for<'a> Fn(&'a Client) -> bool + Clone + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZwlrOutputManagerV1, _>(
            4,
            OutputMngrGlobalData {
                filter: Box::new(client_filter.clone()),
            },
        );

        let extension_global = dh.create_global::<D, ZcosmicOutputManagerV1, _>(
            3,
            OutputMngrGlobalData {
                filter: Box::new(client_filter),
            },
        );

        OutputConfigurationState {
            outputs: Vec::new(),
            removed_outputs: Vec::new(),
            instances: Vec::new(),
            serial_counter: 0,
            global,
            extension_global,
            dh: dh.clone(),
            event_loop_handle: event_loop_handle.clone(),
            _dispatch: std::marker::PhantomData,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    pub fn extension_global_id(&self) -> GlobalId {
        self.extension_global.clone()
    }

    pub fn add_heads<'a>(&mut self, outputs: impl Iterator<Item = &'a Output>) {
        let new_outputs = outputs
            .filter(|o| !self.outputs.contains(o))
            .collect::<Vec<_>>();

        for output in new_outputs {
            let added = output.user_data().insert_if_missing(|| {
                OutputState::new(OutputStateInner {
                    enabled: true,
                    global: None,
                })
            });
            if !added {
                // If head was previous added, enable it again
                let mut inner = output
                    .user_data()
                    .get::<OutputState>()
                    .unwrap()
                    .lock()
                    .unwrap();
                inner.enabled = true;
            }
            self.outputs.push(output.clone());
        }
    }

    pub fn remove_heads<'a>(&mut self, outputs: impl Iterator<Item = &'a Output>) {
        for output in outputs {
            if self.outputs.contains(output) {
                self.removed_outputs.push(output.clone());
                if let Some(inner) = output.user_data().get::<OutputState>() {
                    let mut inner = inner.lock().unwrap();
                    inner.enabled = false;
                    output.leave_all();
                    if let Some(global) = inner.global.take() {
                        remove_global_with_timer(&self.dh, &self.event_loop_handle, global);
                    }
                }
            }
        }
        self.outputs.retain(|x| !self.removed_outputs.contains(x));
    }

    pub fn enable_head(&self, output: &Output) {
        if let Some(inner) = output.user_data().get::<OutputState>() {
            let mut inner = inner.lock().unwrap();
            inner.enabled = true;
        }
    }

    pub fn disable_head(&self, output: &Output) {
        if let Some(inner) = output.user_data().get::<OutputState>() {
            let mut inner = inner.lock().unwrap();
            inner.enabled = false;
        }
    }

    pub fn update(&mut self) {
        self.serial_counter += 1;

        for output in std::mem::take(&mut self.removed_outputs).into_iter() {
            for instance in &mut self.instances {
                let mut removed_heads = Vec::new();
                for head in &mut instance.heads {
                    if &head.output == &output {
                        if head.obj.version() < zwlr_output_head_v1::REQ_RELEASE_SINCE {
                            removed_heads.push(head.obj.clone());
                        }
                        for mode in &mut head.modes {
                            mode.finished();
                        }
                        head.obj.finished();
                        head.finished = true;
                    }
                }
                instance.heads.retain(|h| !removed_heads.contains(&h.obj))
            }
        }

        for output in &self.outputs {
            {
                let state = output.user_data().get::<OutputState>().unwrap();
                let mut inner = state.lock().unwrap();
                if inner.enabled && inner.global.is_none() {
                    inner.global = Some(output.create_global::<D>(&self.dh));
                }
                if !inner.enabled && inner.global.is_some() {
                    remove_global_with_timer(
                        &self.dh,
                        &self.event_loop_handle,
                        inner.global.take().unwrap(),
                    );
                }
            }
            for manager in self.instances.iter_mut() {
                send_head_to_client::<D>(&self.dh, manager, output);
            }
        }

        for manager in self.instances.iter() {
            manager.obj.done(self.serial_counter);
        }
    }

    pub fn outputs(&self) -> impl Iterator<Item = Output> {
        self.outputs.clone().into_iter()
    }
}

fn send_head_to_client<D>(dh: &DisplayHandle, mngr: &mut OutputMngrInstance, output: &Output)
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + Dispatch<ZwlrOutputManagerV1, ()>
        + Dispatch<ZwlrOutputHeadV1, WeakOutput>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    let instance = match mngr
        .heads
        .iter_mut()
        .filter(|i| !i.finished)
        .find(|i| i.output == *output)
    {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.obj.id()) {
                if let Ok(head) = client.create_resource::<ZwlrOutputHeadV1, _, D>(
                    dh,
                    mngr.obj.version(),
                    output.downgrade(),
                ) {
                    mngr.obj.head(&head);
                    let data = OutputHeadInstance {
                        obj: head,
                        extension_obj: None,
                        modes: Vec::new(),
                        output: output.clone(),
                        finished: false,
                    };
                    mngr.heads.push(data);
                    mngr.heads.last_mut().unwrap()
                } else {
                    return;
                }
            } else {
                return;
            }
        }
    };

    instance.obj.name(output.name());
    instance.obj.description(output.description());
    let physical = output.physical_properties();
    if !(physical.size.w == 0 || physical.size.h == 0) {
        instance.obj.physical_size(physical.size.w, physical.size.h);
    }

    let inner = output
        .user_data()
        .get::<OutputState>()
        .unwrap()
        .lock()
        .unwrap();

    let output_modes = output.modes();
    // remove old modes
    instance.modes.retain_mut(|m| {
        if !output_modes.contains(m.data::<Mode>().unwrap()) {
            m.finished();
            false
        } else {
            true
        }
    });

    // update other modes
    for output_mode in output_modes.into_iter() {
        if let Some(mode) = if let Some(wlr_mode) = instance
            .modes
            .iter()
            .find(|mode| *mode.data::<Mode>().unwrap() == output_mode)
        {
            Some(wlr_mode)
        } else if let Ok(client) = dh.get_client(instance.obj.id()) {
            // create the mode if it does not exist yet
            if let Ok(mode) = client.create_resource::<ZwlrOutputModeV1, _, D>(
                dh,
                instance.obj.version(),
                output_mode,
            ) {
                instance.obj.mode(&mode);
                mode.size(output_mode.size.w, output_mode.size.h);
                mode.refresh(output_mode.refresh);
                if output
                    .preferred_mode()
                    .map(|p| p == output_mode)
                    .unwrap_or(false)
                {
                    mode.preferred();
                }
                instance.modes.push(mode);
                instance.modes.last()
            } else {
                None
            }
        } else {
            None
        } {
            if inner.enabled
                && output
                    .current_mode()
                    .map(|c| c == output_mode)
                    .unwrap_or(false)
            {
                instance.obj.current_mode(&*mode);
            }
        }
    }

    instance
        .obj
        .enabled(if inner.enabled || output.mirroring().is_some() {
            1
        } else {
            0
        });
    if inner.enabled || output.mirroring().is_some() {
        let point = output.current_location();
        instance.obj.position(point.x, point.y);
        instance.obj.transform(output.current_transform().into());

        let scale = output.current_scale().fractional_scale();
        instance.obj.scale(scale);

        if instance.obj.version() >= zwlr_output_head_v1::EVT_ADAPTIVE_SYNC_SINCE {
            instance
                .obj
                .adaptive_sync(if output.adaptive_sync() == AdaptiveSync::Disabled {
                    zwlr_output_head_v1::AdaptiveSyncState::Disabled
                } else {
                    zwlr_output_head_v1::AdaptiveSyncState::Enabled
                });
        }

        if let Some(extension_obj) = instance.extension_obj.as_ref() {
            extension_obj.scale_1000((scale * 1000.0).round() as i32);

            extension_obj.mirroring(output.mirroring().map(|o| o.name()));

            if extension_obj.version() >= zcosmic_output_head_v1::EVT_ADAPTIVE_SYNC_EXT_SINCE {
                extension_obj.adaptive_sync_ext(match output.adaptive_sync() {
                    AdaptiveSync::Disabled => {
                        zcosmic_output_head_v1::AdaptiveSyncStateExt::Disabled
                    }
                    AdaptiveSync::Enabled => {
                        zcosmic_output_head_v1::AdaptiveSyncStateExt::Automatic
                    }
                    AdaptiveSync::Force => zcosmic_output_head_v1::AdaptiveSyncStateExt::Always,
                });

                extension_obj.adaptive_sync_available(
                    match output
                        .adaptive_sync_support()
                        .unwrap_or(VrrSupport::NotSupported)
                    {
                        VrrSupport::NotSupported => {
                            zcosmic_output_head_v1::AdaptiveSyncAvailability::Unsupported
                        }
                        VrrSupport::RequiresModeset => {
                            zcosmic_output_head_v1::AdaptiveSyncAvailability::RequiresModeset
                        }
                        VrrSupport::Supported => {
                            zcosmic_output_head_v1::AdaptiveSyncAvailability::Supported
                        }
                    },
                );
            }
        }
    }

    if instance.obj.version() >= zwlr_output_head_v1::EVT_MAKE_SINCE {
        if physical.make != "Unknown" {
            instance.obj.make(physical.make.clone());
        }
        if physical.model != "Unknown" {
            instance.obj.model(physical.model);
        }
        if physical.serial_number != "Unknown" {
            instance.obj.serial_number(physical.serial_number);
        }
    }

    if let Some(extension_obj) = instance.extension_obj.as_ref() {
        if inner.enabled
            && extension_obj.version() >= zcosmic_output_head_v1::EVT_XWAYLAND_PRIMARY_SINCE
        {
            extension_obj.xwayland_primary(output.config().xwayland_primary as u32);
        }
    }
}

fn remove_global_with_timer<D: 'static>(
    dh: &DisplayHandle,
    event_loop_handle: &LoopHandle<D>,
    id: GlobalId,
) {
    dh.disable_global::<D>(id.clone());
    let source = Timer::from_duration(Duration::from_secs(5));
    let dh = dh.clone();
    let res = event_loop_handle.insert_source(source, move |_, _, _state| {
        dh.remove_global::<D>(id.clone());
        TimeoutAction::Drop
    });
    if let Err(err) = res {
        tracing::error!(
            "failed to insert timer source to destroy output global: {}",
            err
        );
    }
}

macro_rules! delegate_output_configuration {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_manager_v1::ZwlrOutputManagerV1: $crate::wayland::protocols::output_configuration::OutputMngrGlobalData
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_manager_v1::ZwlrOutputManagerV1: ()
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_head_v1::ZwlrOutputHeadV1: smithay::output::WeakOutput
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_mode_v1::ZwlrOutputModeV1: smithay::output::Mode
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_configuration_v1::ZwlrOutputConfigurationV1: $crate::wayland::protocols::output_configuration::PendingConfiguration
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_configuration_head_v1::ZwlrOutputConfigurationHeadV1: $crate::wayland::protocols::output_configuration::PendingOutputConfiguration
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::output_management::v1::server::zcosmic_output_manager_v1::ZcosmicOutputManagerV1: $crate::wayland::protocols::output_configuration::OutputMngrGlobalData
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::output_management::v1::server::zcosmic_output_manager_v1::ZcosmicOutputManagerV1: ()
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::output_management::v1::server::zcosmic_output_head_v1::ZcosmicOutputHeadV1: smithay::reexports::wayland_server::Weak<smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_head_v1::ZwlrOutputHeadV1>
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::output_management::v1::server::zcosmic_output_configuration_v1::ZcosmicOutputConfigurationV1: smithay::reexports::wayland_server::Weak<smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_configuration_v1::ZwlrOutputConfigurationV1>
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            cosmic_protocols::output_management::v1::server::zcosmic_output_configuration_head_v1::ZcosmicOutputConfigurationHeadV1: smithay::reexports::wayland_server::Weak<smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_configuration_head_v1::ZwlrOutputConfigurationHeadV1>
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
    };
}
pub(crate) use delegate_output_configuration;

use crate::utils::prelude::OutputExt;
