// SPDX-License-Identifier: GPL-3.0-only

use crate::shell::element::stack::{
    CosmicStackInternal, DefaultDecorations as DefaultStackDecorations, Message as StackMessage,
};
use crate::shell::element::window::{
    CosmicWindowInternal, DefaultDecorations as DefaultWindowDecorations, Message as WindowMessage,
};
use std::sync::{Arc, OnceLock};

/// An _unstable_ interface to customize cosmic-comp at compile-time by providing
/// hooks to be run in specific code paths.
#[derive(Default, Debug, Clone)]
pub struct Hooks {
    pub window_decorations:
        Option<Arc<dyn Decorations<CosmicWindowInternal, WindowMessage> + Send + Sync>>,
    pub stack_decorations:
        Option<Arc<dyn Decorations<CosmicStackInternal, StackMessage> + Send + Sync>>,
}

pub static HOOKS: OnceLock<Hooks> = OnceLock::new();

pub trait Decorations<Internal, Message>: std::fmt::Debug {
    fn view(&self, state: &Internal) -> cosmic::Element<'_, Message>;
}

impl Decorations<CosmicWindowInternal, WindowMessage>
    for Option<Arc<dyn Decorations<CosmicWindowInternal, WindowMessage> + Send + Sync>>
{
    fn view(&self, window: &CosmicWindowInternal) -> cosmic::Element<'_, WindowMessage> {
        match self {
            None => DefaultWindowDecorations.view(window),
            Some(deco) => deco.view(window),
        }
    }
}

impl Decorations<CosmicStackInternal, StackMessage>
    for Option<Arc<dyn Decorations<CosmicStackInternal, StackMessage> + Send + Sync>>
{
    fn view(&self, window: &CosmicStackInternal) -> cosmic::Element<'_, StackMessage> {
        match self {
            None => DefaultStackDecorations.view(window),
            Some(deco) => deco.view(window),
        }
    }
}
