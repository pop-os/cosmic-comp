//! cosmic-comp cosink 适配层入口
//!
//! **详细设计**: doc/spec/DS-202-host-comp-cosmic-adapter.md
//! **协议依赖**: doc/protocol/PROTOCOL-101-COMP.md
//! **开发规范**: doc/OUTLINE.md#第13章-开发规范
//! **代码规范**: doc/OUTLINE.md#第14章-代码规范
//! **术语说明**: doc/OUTLINE.md#第3章-术语说明
//! **DS 同步**: 2025-12-10
//!
//! **实现要点**:
//! - 提供 `CosinkConfig` / `CosinkState` 封装 `CompCore` 与运行状态；
//! - 组合 `CosmicOutputBackend` 与 `CosmicHostdChannel` 实现 DS-201 的 OutputBackend/HostdChannel 适配层；
//! - 为输入管线提供 `handle_input` 入口，将 cosmic-comp 输入事件映射为 `InputEvent` 并注入 `CompCore`。
//!
//! **验收标准**: doc/plan/PHASE-02.md#P2.5

pub mod backend;
pub mod hostd_channel;
pub mod input;

use std::path::PathBuf;

use cosink_comp_core::{
    CompCore, CompError, HostdChannel, InputBackend, InputEvent, InputSink, OutputBackend,
};
use cosink_proto::Frame;

/// cosink 适配配置
#[derive(Debug, Clone)]
pub struct CosinkConfig {
    /// hostd Unix socket 路径，默认 `$XDG_RUNTIME_DIR/cosmic-comp.sock`
    pub hostd_socket_path: PathBuf,
    /// 是否启用 cosink 集成
    pub enabled: bool,
}

impl Default for CosinkConfig {
    fn default() -> Self {
        Self {
            hostd_socket_path: PathBuf::from("/run/user/1000/cosmic-comp.sock"),
            enabled: false,
        }
    }
}

/// cosink 在 cosmic-comp 中的运行状态
pub struct CosinkState<B, C>
where
    B: OutputBackend,
    C: HostdChannel,
{
    /// 核心 CompCore 实例（嵌入式模式，使用 NoInputBackend）
    pub comp_core: CompCore<B, NoInputBackend, C>,
    /// 当前配置
    pub config: CosinkConfig,
    /// 最近错误信息（如有）
    pub last_error: Option<String>,
}

impl<B, C> CosinkState<B, C>
where
    B: OutputBackend,
    C: HostdChannel,
{
    /// 处理来自 hostd 的 COMP_* 帧。
    ///
    /// 实际实现应：
    /// - 调用 `CompCore::handle_hostd_frame(frame)`；
    /// - 根据返回结果更新内部状态与统计信息；
    /// - 在出现错误时记录日志并更新 `last_error`。
    pub async fn handle_hostd_frame(&mut self, frame: Frame) {
        if let Err(err) = self.comp_core.handle_hostd_frame(frame).await {
            self.last_error = Some(err.to_string());
        }
    }

    /// 注入输入事件到 CompCore。
    ///
    /// 实际实现应：
    /// - 将事件转发给 `CompCore::on_input_event(event)`；
    /// - 在错误时记录日志并更新 `last_error`，但不得 panic。
    pub async fn on_input_event(&mut self, event: InputEvent) {
        if let Err(err) = self.comp_core.on_input_event(event).await {
            self.last_error = Some(err.to_string());
        }
    }
}

/// 嵌入式模式下的空输入后端。
///
/// cosmic-comp 已经在自身事件循环中处理输入事件，因此在 cosink 适配层中
/// 不需要额外的 InputBackend 线程/任务，只需要一个占位实现。
pub struct NoInputBackend;

#[async_trait::async_trait]
impl InputBackend for NoInputBackend {
    async fn run_input_loop(&self, _sink: InputSink) -> Result<(), CompError> {
        // 嵌入式模式下不会调用该方法。
        Ok(())
    }
}

/// 适配层错误类型。
///
/// 具体字段与分类将在后续实现阶段根据 DS-202 第6章细化。
#[derive(Debug, thiserror::Error)]
pub enum CosinkAdapterError {
    /// 配置错误：禁用 cosink 或者缺失必要参数。
    #[error("cosink configuration error: {0}")]
    Config(&'static str),

    /// 协议或状态机错误。
    #[error("cosink protocol error: {0}")]
    Protocol(&'static str),

    /// IO 或 socket 相关错误。
    #[error("cosink io error: {0}")]
    Io(#[from] std::io::Error),

    /// CompCore 错误包装。
    #[error("comp-core error: {0}")]
    Comp(#[from] CompError),
}

/// 适配层运行状态。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CosinkRuntimeState {
    Disabled,
    Connecting,
    Connected,
    Degraded,
}

/// 适配层运行统计信息。
#[derive(Debug, Default, Clone)]
pub struct CosinkStats {
    pub frames_total: u64,
    pub frames_with_damage: u64,
    pub input_events_total: u64,
    pub last_error_at: Option<std::time::Instant>,
}

/// 根据配置与运行环境决定是否初始化 cosink 适配层。
///
/// 实际签名会在集成到 cosmic-comp 时根据 `lib.rs::run` 的参数调整。
pub fn maybe_init<B, C>(
    _config: &CosinkConfig,
    _backend: B,
    _channel: C,
) -> Result<Option<CosinkState<B, C>>, CosinkAdapterError>
where
    B: OutputBackend,
    C: HostdChannel,
{
    if !_config.enabled {
        return Ok(None);
    }
    let core = CompCore::new(_backend, NoInputBackend, _channel);
    Ok(Some(CosinkState {
        comp_core: core,
        config: _config.clone(),
        last_error: None,
    }))
}
