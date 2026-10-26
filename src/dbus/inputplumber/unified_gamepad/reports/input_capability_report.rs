use std::{array::TryFromSliceError, fmt::Display};

use packed_struct::{PackedStructInfo, prelude::*};
use thiserror::Error;

use super::super::{
    capability::InputCapability,
    value::{
        BoolValue, Int16Vector3Value, TouchValue, UInt8Value, UInt16Value, UInt16Vector2Value,
        Value,
    },
};

use super::{
    ReportType, UNIFIED_SPEC_VERSION_MAJOR, UNIFIED_SPEC_VERSION_MINOR, ValueType,
    input_data_report::InputDataReport,
};

/// The maximum size of the [InputCapabilityReport]
pub const INPUT_CAPABILITY_REPORT_SIZE: usize = 1274;
/// Maximum number of capabilities supported by the [InputCapabilityReport]
pub const INPUT_CAPABILITY_REPORT_MAX_CAPABILITIES: usize = u8::MAX as usize;

/// The [InputCapabilityReportHeader] defines the header for the [InputCapabilityReport]
/// that is used to describe the input capabilities of the device and how to decode
/// the [InputDataReport].
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "4")]
pub struct InputCapabilityReportHeader {
    /// Major version indicates whether or not compatibility-breaking changes
    /// have occurred.
    #[packed_field(bytes = "0")]
    pub major_ver: u8,
    /// The minor version indicates what capabilities are available
    #[packed_field(bytes = "1")]
    pub minor_ver: u8,
    /// The report type
    #[packed_field(bytes = "2", ty = "enum")]
    pub report_type: ReportType,

    /// The number of input capabilities the device supports
    #[packed_field(bytes = "3")]
    pub capabilities_count: u8,
}

impl Default for InputCapabilityReportHeader {
    fn default() -> Self {
        Self {
            major_ver: UNIFIED_SPEC_VERSION_MAJOR,
            minor_ver: UNIFIED_SPEC_VERSION_MINOR,
            report_type: ReportType::InputCapabilityReport,
            capabilities_count: Default::default(),
        }
    }
}

/// The [InputCapabilityReport] describes the input capabilities of the device
/// and how to decode the [InputDataReport].
#[derive(Debug, Clone, Default)]
pub struct InputCapabilityReport {
    header: InputCapabilityReportHeader,
    capabilities: Vec<InputCapabilityInfo>,
}

impl Display for InputCapabilityReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let header = format!("{}", self.header);
        let caps: Vec<String> = self
            .capabilities
            .iter()
            .map(|cap| format!("{cap}"))
            .collect();
        let body = caps.join("");

        write!(f, "{header}{body}")
    }
}

/// Result of adding a capability to an [InputCapabilityReport]
type AddCapabilityResult = Result<(), AddCapabilityError>;

/// Possible errors when adding a capability to an [InputCapabilityReport]
#[derive(Error, Debug)]
pub enum AddCapabilityError {
    #[error(
        "adding capability `{capability:?}` would exceed maximum number of capabilities `{size:?}`"
    )]
    MaxCapabilitiesExceeded {
        capability: InputCapability,
        size: usize,
    },
}

impl InputCapabilityReport {
    /// Add the given capability to the capability report
    pub fn add_capability(&mut self, capability: InputCapabilityInfo) -> AddCapabilityResult {
        // Don't add empty capabilities
        if capability.value_type == ValueType::None {
            return Ok(());
        }
        if capability.capability == InputCapability::None {
            return Ok(());
        }
        if self.header.capabilities_count == INPUT_CAPABILITY_REPORT_MAX_CAPABILITIES as u8 {
            return Err(AddCapabilityError::MaxCapabilitiesExceeded {
                capability: capability.capability,
                size: INPUT_CAPABILITY_REPORT_MAX_CAPABILITIES,
            });
        }

        // Ensure that the capability doesn't already exist
        let exists = self
            .capabilities
            .iter()
            .any(|cap| cap.capability == capability.capability);
        if exists {
            return Ok(());
        }

        // Increment the capability count and add the capability
        self.header.capabilities_count += 1;
        self.capabilities.push(capability);

        // Sort the capabilities based on their value type to pack values close
        // to each other.
        self.capabilities
            .sort_by_key(|cap| cap.value_type.order_priority());
        self.update_capability_offsets();

        Ok(())
    }

    /// Removes the given capability from the capability report
    #[allow(dead_code)]
    pub fn remove_capability(&mut self, capability: InputCapability) {
        self.capabilities = self
            .capabilities
            .drain(..)
            .filter(|cap| cap.capability != capability)
            .collect();
        self.update_capability_offsets();
    }

    /// Get the capability information for the given capability
    pub fn get_capability(&self, capability: InputCapability) -> Option<InputCapabilityInfo> {
        self.capabilities
            .iter()
            .find(|info| info.capability == capability)
            .map(|info| info.to_owned())
    }

    /// Return all capabilities in the [InputCapabilityReport]
    pub fn get_capabilities(&self) -> &[InputCapabilityInfo] {
        self.capabilities.as_slice()
    }

    /// Updates the `offset` bits of all capabilities
    fn update_capability_offsets(&mut self) {
        // Update the bit offset of all capabilities based on the value type
        let mut offset = 0;
        for cap in self.capabilities.iter_mut() {
            if cap.value_type == ValueType::None {
                continue;
            }

            // Non-bool values should be byte-aligned
            let offset_remainder = offset % 8;
            if cap.value_type != ValueType::Bool && offset_remainder != 0 {
                offset += offset_remainder;
            }

            // Update the offset
            cap.offset = offset;
            let value_size = cap.value_type.size_bits() as u16;
            offset += value_size;
        }
    }
}

/// Result of decoding an [InputDataReport]
type InputDecodeResult = Result<Vec<Value>, InputDecodeError>;

/// Possible errors when decoding an [InputDataReport] from an [InputCapabilityReport]
#[derive(Error, Debug)]
pub enum InputDecodeError {
    #[error(
        "failed to get an appropriately sized slice to decode value from input data report: {0}"
    )]
    BadOffset(#[from] TryFromSliceError),
    #[error("failed to unpack bytes to the appropriate value: {0}")]
    UnpackingFailure(#[from] PackingError),
}

impl InputCapabilityReport {
    /// Decode the given [InputDataReport] according to the capability report
    pub fn decode_data_report(&self, report: &InputDataReport) -> InputDecodeResult {
        let mut values = Vec::with_capacity(self.capabilities.len());
        for capability_info in self.capabilities.iter() {
            // Get the value type and its size
            let value_type = capability_info.value_type;
            let value_size_bytes = value_type.size_bytes();

            // Get the bit/byte offset
            let offset_bits = capability_info.offset as usize;
            let offset_remainder = offset_bits % 8;
            let offset_bytes = (offset_bits - offset_remainder) / 8;

            // Get the byte start and end
            let byte_start = offset_bytes;
            let byte_end = byte_start + value_size_bytes;

            let value = match value_type {
                ValueType::None => Value::None,
                ValueType::Bool => {
                    let byte = report.data[byte_start];
                    // Check if the `offset_remainder` bit in the byte is set
                    let value = byte & (1 << offset_remainder) > 0;
                    Value::Bool(BoolValue { value })
                }
                ValueType::UInt8 => {
                    let slice = &report.data[byte_start..byte_end];
                    let buffer = slice.try_into()?;
                    let value = UInt8Value::unpack(buffer)?;
                    Value::UInt8(value)
                }
                ValueType::UInt16 => {
                    let slice = &report.data[byte_start..byte_end];
                    let buffer = slice.try_into()?;
                    let value = UInt16Value::unpack(buffer)?;
                    Value::UInt16(value)
                }
                ValueType::UInt32 => todo!(),
                ValueType::UInt64 => todo!(),
                ValueType::Int8 => todo!(),
                ValueType::Int16 => todo!(),
                ValueType::Int32 => todo!(),
                ValueType::Int64 => todo!(),
                ValueType::UInt8Vector2 => todo!(),
                ValueType::UInt16Vector2 => {
                    let slice = &report.data[byte_start..byte_end];
                    let buffer = slice.try_into()?;
                    let value = UInt16Vector2Value::unpack(buffer)?;
                    Value::UInt16Vector2(value)
                }
                ValueType::UInt32Vector2 => todo!(),
                ValueType::UInt64Vector2 => todo!(),
                ValueType::Int8Vector2 => todo!(),
                ValueType::Int16Vector2 => todo!(),
                ValueType::Int32Vector2 => todo!(),
                ValueType::Int64Vector2 => todo!(),
                ValueType::UInt8Vector3 => todo!(),
                ValueType::UInt16Vector3 => todo!(),
                ValueType::UInt32Vector3 => todo!(),
                ValueType::UInt64Vector3 => todo!(),
                ValueType::Int8Vector3 => todo!(),
                ValueType::Int16Vector3 => {
                    let slice = &report.data[byte_start..byte_end];
                    let buffer = slice.try_into()?;
                    let value = Int16Vector3Value::unpack(buffer)?;
                    Value::Int16Vector3(value)
                }
                ValueType::Int32Vector3 => todo!(),
                ValueType::Int64Vector3 => todo!(),
                ValueType::Touch => {
                    let slice = &report.data[byte_start..byte_end];
                    let buffer = slice.try_into()?;
                    let value = TouchValue::unpack(buffer)?;
                    Value::Touch(value)
                }
            };
            values.push(value);
        }

        Ok(values)
    }
}

impl InputCapabilityReport {
    /// Packs the structure into a byte array
    pub fn pack_to_vec(&self) -> Result<Vec<u8>, PackingError> {
        // Allocate the packed data based on the header and capabilities added
        let header_size_bytes = InputCapabilityReportHeader::packed_bits() / 8;
        let caps_size_bytes = self.capabilities.len() * (InputCapabilityInfo::packed_bits() / 8);
        let capacity = header_size_bytes + caps_size_bytes;
        let mut data = Vec::with_capacity(capacity);

        // Pack the header bytes
        let header = self.header.pack()?;
        data.extend_from_slice(&header);

        // Pack the body bytes
        for capability in self.capabilities.iter() {
            let packed = capability.pack()?;
            data.extend_from_slice(&packed);
        }

        Ok(data)
    }

    /// Unpacks the structure from the byte array
    pub fn unpack(src: &[u8]) -> Result<Self, PackingError> {
        // Unpack the header
        let header_size_bytes = InputCapabilityReportHeader::packed_bits() / 8;
        let slice = &src[0..header_size_bytes];
        let buffer = slice
            .try_into()
            .map_err(|_err| PackingError::BufferTooSmall)?;
        let header = InputCapabilityReportHeader::unpack(buffer)?;

        // Unpack the body based on the number of capabilities
        let num_capabilities = header.capabilities_count as usize;
        let capability_size_bytes = InputCapabilityInfo::packed_bits() / 8;
        let mut capabilities = Vec::with_capacity(num_capabilities);
        let mut byte_start = header_size_bytes;
        for _ in 0..num_capabilities {
            let byte_end = byte_start + capability_size_bytes;
            let slice = &src[byte_start..byte_end];
            let buffer = slice
                .try_into()
                .map_err(|_err| PackingError::BufferTooSmall)?;
            let capability = InputCapabilityInfo::unpack(buffer)?;
            capabilities.push(capability);
            byte_start = byte_end;
        }

        Ok(Self {
            header,
            capabilities,
        })
    }
}

/// [InputCapabilityInfo] describes a single input capability that a device supports
/// and how to decode the value in the input data report. A consuming driver can
/// use the offset to look for the value at a specific bit offset and can use the
/// value type to determine how to unpack the value.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq, Default)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "5")]
pub struct InputCapabilityInfo {
    /// The capability
    #[packed_field(bytes = "0..=1", endian = "lsb", ty = "enum")]
    pub capability: InputCapability,
    /// The type of value this capability emits
    #[packed_field(bytes = "2", ty = "enum")]
    pub value_type: ValueType,
    /// The bit offset in the input report to read the value for this capability.
    #[packed_field(bytes = "3..=4", endian = "lsb")]
    pub offset: u16,
}

impl InputCapabilityInfo {
    pub fn new(capability: InputCapability, value_type: ValueType) -> Self {
        Self {
            capability,
            value_type,
            offset: 0,
        }
    }
}
