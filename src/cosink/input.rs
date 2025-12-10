//! cosmic-comp 输入事件适配（CosmicInputAdapter）
//!
//! **详细设计**: doc/spec/DS-202-host-comp-cosmic-adapter.md
//! **协议依赖**: doc/protocol/PROTOCOL-101-COMP.md
//! **开发规范**: doc/OUTLINE.md#第13章-开发规范
//! **代码规范**: doc/OUTLINE.md#第14章-代码规范
//! **术语说明**: doc/OUTLINE.md#第3章-术语说明
//! **DS 同步**: 2025-12-10
//!
//! **实现要点**:
//! - 在输入管线中，当事件命中 RemoteOutput 区域时调用本模块；
//! - 将 cosmic-comp 的键盘/指针/滚轮事件映射为 DS-201 定义的 `InputEvent`；
//! - 通过 `CosinkState::on_input_event` 注入到 CompCore，同时遵循 dispatch_local/dispatch_remote 策略。

use cosink_comp_core::{CompError, InputEvent};

/// 根据 cosmic-comp 输入事件构造并注入 InputEvent。
///
/// 注意：`event` 的实际类型需在后续集成阶段根据 cosmic-comp 现有输入管线替换。
pub fn handle_input(
    _common: &mut crate::state::Common,
    // 这里的事件类型根据 cosmic-comp 实际定义替换
    _event: crate::input::InputEvent<crate::backend::BackendInput>,
) -> Result<(), CompError> {
    // TODO:
    // 1. 判断事件是否命中被选为 RemoteOutput 的输出/区域；
    // 2. 根据事件类型构造 `InputEvent`（Key/MouseMove/MouseButton/Scroll 等）；
    // 3. 调用 `CosinkState::on_input_event` 注入到 CompCore。
    Ok(())
}

