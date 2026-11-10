use std::pin::Pin;

use crate::{Error, Message};

use futures_util::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

pub trait Readable {
    fn readable(&self) -> impl Future<Output = std::io::Result<()>>;
}

/// Encode and write a cosmic-comp ipc message to a given `futures_io` compatible writer.
pub async fn write_msg<W: AsyncWrite>(mut writer: Pin<&mut W>, msg: &Message) -> Result<(), Error> {
    let message = serde_json::to_string(msg).map_err(Error::Serialize)?;
    let bytes = message.into_bytes();
    let len = (bytes.len() as u16).to_ne_bytes();
    writer.write_all(&len).await?;
    writer.write_all(&bytes).await?;
    writer.flush().await?;
    Ok(())
}

/// Read and parse a cosmic-comp ipc message from a given `futures_io` compatible reader.
pub async fn read_msg<R: AsyncRead + Readable>(mut reader: Pin<&mut R>) -> Result<Message, Error> {
    reader.readable().await?;
    let mut buf = [0u8; 2];
    reader.read_exact(&mut buf).await?;
    let len = u16::from_ne_bytes(buf);

    reader.readable().await?;
    let mut msg_buf = vec![0u8; len as usize];
    reader.read_exact(&mut msg_buf).await?;
    serde_json::from_slice(&msg_buf).map_err(Error::Deserialize)
}
