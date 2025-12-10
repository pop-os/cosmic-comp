//! cosmic-comp ↔ cosink-hostd Unix socket 适配（CosmicHostdChannel）
//!
//! **详细设计**: doc/spec/DS-202-host-comp-cosmic-adapter.md
//! **协议依赖**: doc/protocol/PROTOCOL-101-COMP.md
//! **开发规范**: doc/OUTLINE.md#第13章-开发规范
//! **代码规范**: doc/OUTLINE.md#第14章-代码规范
//! **术语说明**: doc/OUTLINE.md#第3章-术语说明
//! **DS 同步**: 2025-12-10
//!
//! **实现要点**:
//! - 在 calloop 事件循环中监听 hostd Unix socket；
//! - 使用 `cosink-proto::FrameDecoder` 解析 COMP_* 帧并转发给 `CompCore`；
//! - 在错误时记录日志并优雅关闭连接。

use cosink_comp_core::{CompError, HostdChannel};
use cosink_proto::Frame;

/// cosmic-comp 与 hostd 的 Unix socket 通道。
///
/// 具体字段（socket、decoder 等）在后续实现阶段补充。
pub struct CosmicHostdChannel {
    // TODO: 保存 UnixStream 与 FrameDecoder。
}

#[async_trait::async_trait]
impl HostdChannel for CosmicHostdChannel {
    async fn send(&self, _frame: Frame) -> Result<(), CompError> {
        // TODO: 将 Frame 编码为字节并通过 Unix socket 发送给 hostd。
        Ok(())
    }

    async fn recv(&self) -> Result<Frame, CompError> {
        // TODO: 从 Unix socket 读取并解码一个完整 Frame。
        Err(CompError::Internal(
            "CosmicHostdChannel::recv not implemented yet",
        ))
    }
}

