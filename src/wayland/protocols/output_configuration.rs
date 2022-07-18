// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::{
        wayland_protocols_wlr::output_management::v1::server::{
            zwlr_output_configuration_head_v1::{self, ZwlrOutputConfigurationHeadV1},
            zwlr_output_configuration_v1::{self, ZwlrOutputConfigurationV1},
            zwlr_output_head_v1::{self, ZwlrOutputHeadV1},
            zwlr_output_manager_v1::{self, ZwlrOutputManagerV1},
            zwlr_output_mode_v1::{self, ZwlrOutputModeV1},
        },
        wayland_server::{
            backend::{ClientId, GlobalId, ObjectId},
            protocol::wl_output::WlOutput,
            Client, DataInit, Dispatch, DisplayHandle,
            GlobalDispatch, New, Resource,
        },
    },
    utils::{Logical, Physical, Point, Size, Transform},
    wayland::output::{Mode, Output, OutputData},
};
use std::{
    convert::{TryFrom, TryInto},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
};

pub struct OutputConfigurationState<D> {
    outputs: Vec<Output>,
    removed_outputs: Vec<Output>,
    instances: Vec<OutputMngrInstance>,
    serial_counter: u32,
    global: GlobalId,
    dh: DisplayHandle,
    _dispatch: std::marker::PhantomData<D>,
}

pub trait OutputConfigurationHandler: Sized {
    fn output_configuration_state(&mut self) -> &mut OutputConfigurationState<Self>;

    fn test_configuration(&mut self, conf: Vec<(Output, OutputConfiguration)>) -> bool;
    fn apply_configuration(&mut self, conf: Vec<(Output, OutputConfiguration)>) -> bool;
}

pub struct OutputMngrGlobalData {
    filter: Box<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

struct OutputMngrInstance {
    obj: ZwlrOutputManagerV1,
    active: Arc<AtomicBool>,
    heads: Vec<OutputHeadInstance>,
}

struct OutputHeadInstance {
    output: Output,
    head: ZwlrOutputHeadV1,
    modes: Vec<ZwlrOutputModeV1>,
}

pub struct OutputMngrInstanceData {
    active: Arc<AtomicBool>,
}

#[derive(Debug, Default)]
pub struct PendingConfigurationInner {
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
    mode: Option<ModeConfiguration<ZwlrOutputModeV1>>,
    position: Option<Point<i32, Logical>>,
    transform: Option<Transform>,
    scale: Option<f64>,
}
pub type PendingOutputConfiguration = Mutex<PendingOutputConfigurationInner>;

#[derive(Debug, Clone)]
pub enum OutputConfiguration {
    Enabled {
        mode: Option<ModeConfiguration<Mode>>,
        position: Option<Point<i32, Logical>>,
        transform: Option<Transform>,
        scale: Option<f64>,
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
            position: pending.position,
            transform: pending.transform,
            scale: pending.scale,
        })
    }
}

struct OutputStateInner {
    enabled: bool,
    global: Option<GlobalId>,
}
type OutputState = Mutex<OutputStateInner>;

impl<D> GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData, D>
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
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZwlrOutputManagerV1>,
        _global_data: &OutputMngrGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let active = Arc::new(AtomicBool::new(true));
        let data = OutputMngrInstanceData {
            active: active.clone(),
        };
        let mut instance = OutputMngrInstance {
            obj: data_init.init(resource, data),
            heads: Vec::new(),
            active,
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

impl<D> Dispatch<ZwlrOutputManagerV1, OutputMngrInstanceData, D>
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
        state: &mut D,
        _client: &Client,
        _obj: &ZwlrOutputManagerV1,
        request: zwlr_output_manager_v1::Request,
        data: &OutputMngrInstanceData,
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zwlr_output_manager_v1::Request::CreateConfiguration { id, serial } => {
                let conf = data_init.init(
                    id,
                    PendingConfiguration::new(PendingConfigurationInner {
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
                data.active.store(false, Ordering::SeqCst);
            }
            _ => {}
        }
    }
}

impl<D> Dispatch<ZwlrOutputHeadV1, Output, D> for OutputConfigurationState<D>
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
        _obj: &ZwlrOutputHeadV1,
        request: zwlr_output_head_v1::Request,
        _data: &Output,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            _ => {}
        }
    }

    fn destroyed(state: &mut D, _client: ClientId, resource: ObjectId, _data: &Output) {
        for instance in &mut state.output_configuration_state().instances {
            instance.heads.retain(|h| h.head.id() != resource);
        }
    }
}

impl<D> Dispatch<ZwlrOutputModeV1, Mode, D> for OutputConfigurationState<D>
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
        _obj: &ZwlrOutputModeV1,
        request: zwlr_output_mode_v1::Request,
        _data: &Mode,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            _ => {}
        }
    }
}

impl<D> Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration, D>
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
                    return;
                }

                let final_conf = match pending
                    .heads
                    .iter_mut()
                    .map(|(head, conf)| {
                        let output = match {
                            inner
                                .instances
                                .iter()
                                .find_map(|instance| {
                                    instance.heads.iter().find(|h| h.head == *head)
                                })
                                .map(|i| i.output.clone())
                        } {
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
            }
            zwlr_output_configuration_v1::Request::Destroy => {}
            _ => {}
        }
    }
}

impl<D> Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration, D>
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
            _ => {}
        }
    }
}

impl<D> OutputConfigurationState<D>
where
    D: GlobalDispatch<ZwlrOutputManagerV1, OutputMngrGlobalData>
        + GlobalDispatch<WlOutput, OutputData>
        + Dispatch<ZwlrOutputManagerV1, OutputMngrInstanceData>
        + Dispatch<ZwlrOutputHeadV1, Output>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + Dispatch<ZwlrOutputConfigurationHeadV1, PendingOutputConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    pub fn new<F>(dh: &DisplayHandle, client_filter: F) -> OutputConfigurationState<D>
    where
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let global = dh.create_global::<D, ZwlrOutputManagerV1, _>(
            2,
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
            dh: dh.clone(),
            _dispatch: std::marker::PhantomData,
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    pub fn add_heads<'a>(&mut self, outputs: impl Iterator<Item = &'a Output>) {
        let new_outputs = outputs
            .filter(|o| !self.outputs.contains(o))
            .collect::<Vec<_>>();

        for output in new_outputs {
            output.user_data().insert_if_missing(|| {
                OutputState::new(OutputStateInner {
                    enabled: true,
                    global: None,
                })
            });
            self.outputs.push(output.clone());
        }
    }

    pub fn remove_heads<'a>(&mut self, outputs: impl Iterator<Item = &'a Output>) {
        for output in outputs {
            if self.outputs.contains(output) {
                self.removed_outputs.push(output.clone());
                if let Some(inner) = output.user_data().get::<OutputState>() {
                    let mut inner = inner.lock().unwrap();
                    // if it gets re-added it should start with being enabled and no global
                    inner.enabled = true;
                    if let Some(global) = inner.global.take() {
                        self.dh.remove_global::<D>(global);
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
        self.instances.retain(|x| x.active.load(Ordering::SeqCst));
        self.serial_counter += 1;

        for output in std::mem::take(&mut self.removed_outputs).into_iter() {
            for instance in &mut self.instances {
                instance.heads.retain_mut(|head| {
                    if &head.output == &output {
                        for mode in &head.modes {
                            mode.finished();
                        }
                        head.head.finished();
                        false
                    } else {
                        true
                    }
                });
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
                    self.dh.remove_global::<D>(inner.global.take().unwrap());
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
        + Dispatch<ZwlrOutputManagerV1, OutputMngrInstanceData>
        + Dispatch<ZwlrOutputHeadV1, Output>
        + Dispatch<ZwlrOutputModeV1, Mode>
        + Dispatch<ZwlrOutputConfigurationV1, PendingConfiguration>
        + OutputConfigurationHandler
        + 'static,
{
    let instance = match mngr.heads.iter_mut().find(|i| i.output == *output) {
        Some(i) => i,
        None => {
            if let Ok(client) = dh.get_client(mngr.obj.id()) {
                if let Ok(head) = client.create_resource::<ZwlrOutputHeadV1, _, D>(
                    dh,
                    mngr.obj.version(),
                    output.clone(),
                ) {
                    mngr.obj.head(&head);
                    let data = OutputHeadInstance {
                        head,
                        modes: Vec::new(),
                        output: output.clone(),
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

    instance.head.name(output.name());
    instance.head.description(output.description());
    let physical = output.physical_properties();
    if !(physical.size.w == 0 || physical.size.h == 0) {
        instance
            .head
            .physical_size(physical.size.w, physical.size.h);
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
        } else if let Ok(client) = dh.get_client(instance.head.id()) {
            // create the mode if it does not exist yet
            if let Ok(mode) = client.create_resource::<ZwlrOutputModeV1, _, D>(
                dh,
                instance.head.version(),
                output_mode,
            ) {
                instance.head.mode(&mode);
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
                instance.head.current_mode(&*mode);
            }
        }
    }

    instance.head.enabled(if inner.enabled { 1 } else { 0 });
    if inner.enabled {
        let point = output.current_location();
        instance.head.position(point.x, point.y);
        instance.head.transform(output.current_transform());
        instance
            .head
            .scale(output.current_scale().fractional_scale());
    }

    if mngr.obj.version() >= zwlr_output_head_v1::EVT_MAKE_SINCE {
        if physical.make != "Unknown" {
            instance.head.make(physical.make.clone());
        }
        if physical.model != "Unknown" {
            instance.head.model(physical.model);
        }
    }
}

macro_rules! delegate_output_configuration {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_manager_v1::ZwlrOutputManagerV1: $crate::wayland::protocols::output_configuration::OutputMngrGlobalData
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_manager_v1::ZwlrOutputManagerV1: $crate::wayland::protocols::output_configuration::OutputMngrInstanceData
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_head_v1::ZwlrOutputHeadV1: smithay::wayland::output::Output
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_mode_v1::ZwlrOutputModeV1: smithay::wayland::output::Mode
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_configuration_v1::ZwlrOutputConfigurationV1: $crate::wayland::protocols::output_configuration::PendingConfiguration
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::output_management::v1::server::zwlr_output_configuration_head_v1::ZwlrOutputConfigurationHeadV1: $crate::wayland::protocols::output_configuration::PendingOutputConfiguration
        ] => $crate::wayland::protocols::output_configuration::OutputConfigurationState<Self>);
    };
}
pub(crate) use delegate_output_configuration;
