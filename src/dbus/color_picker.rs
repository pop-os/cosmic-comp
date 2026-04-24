// SPDX-License-Identifier: GPL-3.0-only
//
// DBus interface `com.system76.CosmicComp.ColorPicker` at
// `/com/system76/CosmicComp/ColorPicker`.
//
// One method: `PickPixel(output: s, x: i32, y: i32) -> (r: d, g: d, b: d)`.
//
// Reserved for xdg-desktop-portal-cosmic (gated via NameOwners)

use calloop::channel::Sender;
use futures_channel::oneshot;
use futures_executor::ThreadPool;
use std::sync::{Arc, OnceLock};
use zbus::{fdo, message::Header, names::WellKnownName};

use super::name_owners::NameOwners;

const ALLOWED_NAMES: &[WellKnownName<'static>] = &[WellKnownName::from_static_str_unchecked(
    "org.freedesktop.impl.portal.desktop.cosmic",
)];

pub enum Request {
    PickPixel {
        output: String,
        x: i32,
        y: i32,
        reply: oneshot::Sender<Result<(f64, f64, f64), String>>,
    },
}

#[derive(Debug)]
pub struct ColorPickerState {
    #[allow(dead_code)]
    conn: Arc<OnceLock<zbus::Connection>>,
    #[allow(dead_code)]
    name_owners: Arc<OnceLock<NameOwners>>,
}

impl ColorPickerState {
    pub fn new(executor: &ThreadPool, tx: Sender<Request>) -> Self {
        let conn_cell = Arc::new(OnceLock::new());
        let conn_cell_clone = conn_cell.clone();
        let name_owners_cell = Arc::new(OnceLock::new());
        let name_owners_cell_clone = name_owners_cell.clone();
        let executor_clone = executor.clone();
        executor.spawn_ok(async move {
            match serve(tx, &executor_clone).await {
                Ok((conn, name_owners)) => {
                    let _ = conn_cell_clone.set(conn);
                    let _ = name_owners_cell_clone.set(name_owners);
                }
                Err(err) => {
                    tracing::error!("Failed to serve `com.system76.CosmicComp.ColorPicker`: {err}");
                }
            }
        });
        Self {
            conn: conn_cell,
            name_owners: name_owners_cell,
        }
    }
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
    async fn pick_pixel(
        &self,
        #[zbus(header)] header: Header<'_>,
        output: String,
        x: i32,
        y: i32,
    ) -> fdo::Result<(f64, f64, f64)> {
        let sender = header
            .sender()
            .ok_or_else(|| fdo::Error::AccessDenied("Missing sender".to_string()))?;
        self.check_sender_allowed(sender).await?;

        let (reply_tx, reply_rx) = oneshot::channel();
        self.tx
            .send(Request::PickPixel {
                output,
                x,
                y,
                reply: reply_tx,
            })
            .map_err(|e| fdo::Error::Failed(format!("Failed to dispatch: {e}")))?;
        reply_rx
            .await
            .map_err(|_| fdo::Error::Failed("Reply cancelled".to_string()))?
            .map_err(fdo::Error::Failed)
    }
}

async fn serve(
    tx: Sender<Request>,
    executor: &ThreadPool,
) -> zbus::Result<(zbus::Connection, NameOwners)> {
    let conn = zbus::Connection::session().await?;
    let name_owners = NameOwners::new(&conn, executor).await?;
    let color_picker = ColorPicker {
        tx,
        name_owners: name_owners.clone(),
    };
    conn.object_server()
        .at("/com/system76/CosmicComp/ColorPicker", color_picker)
        .await?;
    conn.request_name("com.system76.CosmicComp").await?;
    Ok((conn, name_owners))
}
