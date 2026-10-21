use packed_struct::prelude::*;
use thiserror::Error;

use super::super::{capability::InputCapability, value::TouchValue};

use super::{
    ReportType, UNIFIED_SPEC_VERSION_MAJOR, UNIFIED_SPEC_VERSION_MINOR, ValueType,
    input_capability_report::InputCapabilityReport,
};

/// The maximum size of the [InputDataReport]
pub const INPUT_DATA_REPORT_SIZE: usize = 68;

/// The [InputDataReport] contains input data based on the capabilities of the
/// device according to the [super::input_capability_report::InputCapabilityReport].
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "68")]
pub struct InputDataReport {
    // byte 0-3
    #[packed_field(bytes = "0")]
    pub major_ver: u8, // Major version? Always 0x01
    #[packed_field(bytes = "1")]
    pub minor_ver: u8, // Minor version? Always 0x00
    #[packed_field(bytes = "2", ty = "enum")]
    pub report_type: ReportType, // Report type? Always 0x09

    /// The state version will increment whenever a state change has occurred.
    /// If the version has not changed since the last report, there is no need
    /// to further process the report.
    #[packed_field(bytes = "4..=5", endian = "lsb")]
    pub state_version: u16,

    /// Input data can be decoded by reading capabilities in the [super::input_capability_report::InputCapabilityReport].
    #[packed_field(bytes = "6..=67")]
    pub data: [u8; 62],
}

impl Default for InputDataReport {
    fn default() -> Self {
        Self {
            major_ver: UNIFIED_SPEC_VERSION_MAJOR,
            minor_ver: UNIFIED_SPEC_VERSION_MINOR,
            report_type: ReportType::InputDataReport,
            state_version: Default::default(),
            data: [0; 62],
        }
    }
}

/// Result of updating an [InputDataReport]
type UpdateInputReportResult = Result<(), UpdateInputReportError>;

/// Possible errors when updating a [InputDataReport]
#[derive(Error, Debug)]
pub enum UpdateInputReportError {
    #[error("capability `{0:?}` was not found in the input capability report")]
    CapabilityNotFound(InputCapability),
    #[error(
        "value type in capability report `{capability_type:?}` does not match the value type in the update `{update_type:?}`"
    )]
    ValueTypeMismatch {
        capability_type: ValueType,
        update_type: ValueType,
    },
    #[error("unable to pack update values: {0:?}")]
    PackingError(#[from] PackingError),
    #[error(
        "update for capability `{capability:?}` would update report at byte `{offset:?}` which exceeds the max size of the input report `{size:?}`"
    )]
    ReportSizeExceeded {
        capability: InputCapability,
        offset: usize,
        size: usize,
    },
}

impl InputDataReport {
    /// Update the [InputDataReport] with the given value based on the [InputCapabilityReport]
    pub fn update(
        &mut self,
        capability_report: &InputCapabilityReport,
        update: StateUpdate,
    ) -> UpdateInputReportResult {
        let Some(capability_info) = capability_report.get_capability(update.capability) else {
            return Err(UpdateInputReportError::CapabilityNotFound(
                update.capability,
            ));
        };

        // Validate that the event value matches the value type
        let capability_type = capability_info.value_type;
        let update_type = update.value.value_type();
        if capability_type != update_type {
            return Err(UpdateInputReportError::ValueTypeMismatch {
                capability_type,
                update_type,
            });
        }

        // Get the value type and its size
        let value_type = capability_info.value_type;
        let value_size_bytes = value_type.size_bytes();

        // Get the bit/byte offset
        let offset_bits = capability_info.offset as usize;
        let offset_remainder = offset_bits % 8;
        let offset_bytes = (offset_bits - offset_remainder) / 8;

        // Get the byte start
        let byte_start = offset_bytes;

        // Ensure the data can fit in the report
        if byte_start + value_size_bytes > self.data.len() {
            return Err(UpdateInputReportError::ReportSizeExceeded {
                capability: update.capability,
                offset: byte_start + value_size_bytes,
                size: self.data.len(),
            });
        }

        match update.value {
            ValueUpdate::None => (),
            ValueUpdate::Bool(value) => {
                // Shift the value to the appropriate bit offset
                if value.value {
                    // Toggle on the bit at the offset remainder
                    self.data[byte_start] |= 1 << offset_remainder;
                } else {
                    // Toggle off the bit at the offset remainder
                    self.data[byte_start] ^= 1 << offset_remainder;
                }
            }
            ValueUpdate::UInt8(value) => {
                self.data[byte_start] = value.value;
            }
            ValueUpdate::UInt16(value) => {
                let data = value.value.to_le_bytes();
                self.data[byte_start] = data[0];
                self.data[byte_start + 1] = data[1];
            }
            ValueUpdate::UInt16Vector2(value) => {
                let x_data = value.x.map(|x| x.to_le_bytes());
                if let Some(data) = x_data {
                    self.data[byte_start] = data[0];
                    self.data[byte_start + 1] = data[1];
                }

                let y_data = value.y.map(|y| y.to_le_bytes());
                if let Some(data) = y_data {
                    self.data[byte_start + 2] = data[0];
                    self.data[byte_start + 3] = data[1];
                }
            }
            ValueUpdate::Int16Vector3(value) => {
                let x_data = value.x.map(|x| x.to_le_bytes());
                if let Some(data) = x_data {
                    self.data[byte_start] = data[0];
                    self.data[byte_start + 1] = data[1];
                }

                let y_data = value.y.map(|y| y.to_le_bytes());
                if let Some(data) = y_data {
                    self.data[byte_start + 2] = data[0];
                    self.data[byte_start + 3] = data[1];
                }

                let z_data = value.z.map(|z| z.to_le_bytes());
                if let Some(data) = z_data {
                    self.data[byte_start + 4] = data[0];
                    self.data[byte_start + 5] = data[1];
                }
            }
            ValueUpdate::Touch(value) => {
                let data = value.pack()?;
                self.data[byte_start] = data[0];
                self.data[byte_start + 1] = data[1];
                self.data[byte_start + 2] = data[2];
                self.data[byte_start + 3] = data[3];
                self.data[byte_start + 4] = data[4];
                self.data[byte_start + 5] = data[5];
            }
        };

        self.state_version = self.state_version.wrapping_add(1);
        tracing::trace!("Updated data: {:?}", self.data);

        Ok(())
    }
}

/// [StateUpdate] is used to provide information on how to update the state of
/// a unified gamepad device.
#[derive(Clone, Debug, Default)]
pub struct StateUpdate {
    pub capability: InputCapability,
    pub value: ValueUpdate,
}

#[derive(Debug, Clone, Default)]
pub enum ValueUpdate {
    #[default]
    None,
    Bool(BoolUpdate),
    UInt8(UInt8Update),
    #[allow(dead_code)]
    UInt16(UInt16Update),
    UInt16Vector2(UInt16Vector2Update),
    Int16Vector3(Int16Vector3Update),
    Touch(TouchValue),
}

impl ValueUpdate {
    /// Return the [ValueType] for this [Value]
    pub fn value_type(&self) -> ValueType {
        match self {
            Self::None => ValueType::None,
            Self::Bool(_) => ValueType::Bool,
            Self::UInt8(_) => ValueType::UInt8,
            Self::UInt16(_) => ValueType::UInt16,
            Self::UInt16Vector2(_) => ValueType::UInt16Vector2,
            Self::Int16Vector3(_) => ValueType::Int16Vector3,
            Self::Touch(_) => ValueType::Touch,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BoolUpdate {
    pub value: bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct UInt8Update {
    pub value: u8,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct UInt16Update {
    pub value: u16,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct UInt16Vector2Update {
    pub x: Option<u16>,
    pub y: Option<u16>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Int16Vector3Update {
    pub x: Option<i16>,
    pub y: Option<i16>,
    pub z: Option<i16>,
}
