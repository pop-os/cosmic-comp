//! cosmic-comp 输出后端适配（CosmicOutputBackend）
//!
//! **详细设计**: doc/spec/DS-202-host-comp-cosmic-adapter.md
//! **协议依赖**: doc/protocol/PROTOCOL-101-COMP.md
//! **开发规范**: doc/OUTLINE.md#第13章-开发规范
//! **代码规范**: doc/OUTLINE.md#第14章-代码规范
//! **术语说明**: doc/OUTLINE.md#第3章-术语说明
//! **DS 同步**: 2025-12-10
//!
//! **实现要点**:
//! - 绑定一个被 cosink 使用的 RemoteOutput，复用现有渲染管线产生 dmabuf；
//! - 在 `begin_frame` / `end_frame` 中衔接 smithay 的输出与 DS-201 的 `DmabufDesc`；
//! - 在 DAMAGE 为空时返回空的 DamageRegion 以便 hostd 跳过编码。

use cosink_comp_core::{CompError, DamageRegion, DmabufDesc, OutputBackend};

/// cosmic-comp 输出后端适配。
///
/// 具体字段在后续实现中根据 backend/kms/x11/winit 的结构补充。
pub struct CosmicOutputBackend {
    // TODO: 绑定 smithay Output / renderer / screencopy 状态。
}

#[async_trait::async_trait]
impl OutputBackend for CosmicOutputBackend {
    type BufferHandle = ();

    async fn create_output(&self, _width: u32, _height: u32) -> Result<(), CompError> {
        // TODO: 选择/创建供 cosink 使用的输出，并校验分辨率。
        Ok(())
    }

    async fn destroy_output(&self) -> Result<(), CompError> {
        // TODO: 释放与 cosink 相关的输出资源，但不修改原有输出拓扑。
        Ok(())
    }

    async fn begin_frame(&self) -> Result<Self::BufferHandle, CompError> {
        // TODO: 准备一帧渲染上下文（offscreen 或指定输出）。
        Ok(())
    }

    async fn end_frame(
        &self,
        _buffer: Self::BufferHandle,
        _damage: DamageRegion,
    ) -> Result<DmabufDesc, CompError> {
        // TODO: 完成渲染并导出 dmabuf，构造 DmabufDesc。
        Err(CompError::Internal("CosmicOutputBackend::end_frame not implemented yet"))
    }
}

