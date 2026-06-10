// SPDX-License-Identifier: GPL-3.0-only
//
// DBus interface `com.system76.CosmicComp.ColorPicker` at
// `/com/system76/CosmicComp/ColorPicker`.
//
// One method: `ColorUnderCursor() -> (r: d, g: d, b: d)`, which samples the
// colour of the pixel directly under the compositor's current pointer. The
// caller sends no coordinates: the compositor is the only component that knows
// the live pointer position and the active screen-magnifier transform, so it
// resolves everything itself (robust to zoom, fractional scale, and rotation).
//
// Reserved for xdg-desktop-portal-cosmic (gated via NameOwners)

use calloop::channel::Sender;
use futures_channel::oneshot;
use zbus::{fdo, message::Header, names::WellKnownName};

use super::name_owners::NameOwners;

const ALLOWED_NAMES: &[WellKnownName<'static>] = &[WellKnownName::from_static_str_unchecked(
    "org.freedesktop.impl.portal.desktop.cosmic",
)];

#[derive(Debug)]
pub enum Request {
    ColorUnderCursor {
        reply: oneshot::Sender<Result<(f64, f64, f64), String>>,
    },
}

struct ColorPicker {
    tx: Sender<Request>,
    name_owners: NameOwners,
}

impl ColorPicker {
    async fn check_sender_allowed(&self, sender: &zbus::names::UniqueName<'_>) -> fdo::Result<()> {
        if self.name_owners.check_owner(sender, ALLOWED_NAMES).await {
            Ok(())
        } else {
            Err(fdo::Error::AccessDenied("Access denied".to_string()))
        }
    }
}

#[zbus::interface(name = "com.system76.CosmicComp.ColorPicker")]
impl ColorPicker {
    async fn color_under_cursor(
        &self,
        #[zbus(header)] header: Header<'_>,
    ) -> fdo::Result<(f64, f64, f64)> {
        let sender = header
            .sender()
            .ok_or_else(|| fdo::Error::AccessDenied("Missing sender".to_string()))?;
        self.check_sender_allowed(sender).await?;

        let (reply_tx, reply_rx) = oneshot::channel();
        self.tx
            .send(Request::ColorUnderCursor { reply: reply_tx })
            .map_err(|e| fdo::Error::Failed(format!("Failed to dispatch: {e}")))?;
        reply_rx
            .await
            .map_err(|_| fdo::Error::Failed("Reply cancelled".to_string()))?
            .map_err(fdo::Error::Failed)
    }
}

/// Register the `com.system76.CosmicComp.ColorPicker` interface on the shared
/// session connection. `tx` dispatches [`Request`]s to the main thread, which
/// owns the renderer needed to sample a pixel.
pub async fn init(
    conn: &zbus::Connection,
    name_owners: &NameOwners,
    tx: Sender<Request>,
) -> zbus::Result<()> {
    let color_picker = ColorPicker {
        tx,
        name_owners: name_owners.clone(),
    };
    conn.object_server()
        .at("/com/system76/CosmicComp/ColorPicker", color_picker)
        .await?;
    conn.request_name("com.system76.CosmicComp").await?;
    Ok(())
}
