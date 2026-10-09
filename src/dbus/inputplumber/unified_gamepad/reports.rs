use packed_struct::{PackedStructInfo, prelude::*};

use super::value::{
    BoolValue, Int16Vector3Value, TouchValue, UInt8Value, UInt16Value, UInt16Vector2Value,
};

pub mod input_capability_report;
pub mod input_data_report;

/// Major version of the Unified Controller Input Specification that this
/// implementation supports.
pub const UNIFIED_SPEC_VERSION_MAJOR: u8 = 1;
/// Minor version of the Unified Controller Input Specification that this
/// implementation supports.
pub const UNIFIED_SPEC_VERSION_MINOR: u8 = 0;

/// Report descriptor to advertise
pub const REPORT_DESCRIPTOR: [u8; 24] = [
    // report descriptor for general input/output
    0x06, 0x00, 0xFF, // Usage Page (Vendor Defined 0xFF00)
    0x09, 0x01, // Usage (0x01)
    0xA1, 0x01, // Collection (Application)
    0x09, 0x02, //   Usage (0x02)
    0x15, 0x00, //   Logical Minimum (0)
    0x25, 0xFF, //   Logical Maximum (255)
    0x75, 0x08, //   Report Size (8)
    0x95, 0x40, //   Report Count (64)
    0x81, 0x02, //   Input (Data,Var,Abs,No Wrap,Linear,Preferred State,No Null Position)
    0x09, 0x03, //   Usage (0x03)
    0x91,
    0x02, //   Output (Data,Var,Abs,No Wrap,Linear,Preferred State,No Null Position,Non-volatile)
    0xC0, // End Collection
];

/// ReportType contains an enumeration of all possible report types
#[derive(PrimitiveEnum_u8, Clone, Copy, PartialEq, Debug)]
pub enum ReportType {
    Unknown = 0x00,
    InputCapabilityReport = 0x01,
    InputDataReport = 0x02,
    OutputCapabilityReport = 0x03,
    OutputDataReport = 0x04,
}

impl From<u8> for ReportType {
    fn from(value: u8) -> Self {
        match value {
            0x01 => Self::InputCapabilityReport,
            0x02 => Self::InputDataReport,
            0x03 => Self::OutputCapabilityReport,
            0x04 => Self::OutputDataReport,
            _ => Self::Unknown,
        }
    }
}

/// Feature report types
#[derive(PrimitiveEnum_u8, Clone, Copy, PartialEq, Debug)]
#[allow(dead_code)]
pub enum FeatureReportType {
    /// Instruct the driver to return a feature report with the available capabilities
    GetInputCapabilities = 0x01,
    GetOutputCapabilites = 0x02,
    GetName = 0x03,
    GetVendorId = 0x04,
    GetProductId = 0x05,
    GetGlobalProductId = 0x06,
    GetSerial = 0x07,

    // Examples
    InputData = 0x09,
    SetMappings = 0x80,
    ClearMappings = 0x81,
    GetMappings = 0x82,
    GetAttrib = 0x83,
    GetAttribLabel = 0x84,
    DefaultMappings = 0x85,
    FactoryReset = 0x86,
    WriteRegister = 0x87,
    ClearRegister = 0x88,
    ReadRegister = 0x89,
    GetRegisterLabel = 0x8a,
    GetRegisterMax = 0x8b,
    GetRegisterDefault = 0x8c,
    SetMode = 0x8d,
    DefaultMouse = 0x8e,
    TriggerHapticPulse = 0x8f,
    RequestCommStatus = 0xb4,
    //GetSerial = 0xae,
    TriggerHapticCommand = 0xea,
    TriggerRumbleCommand = 0xeb,
}

// Describes how to decode a particular value
#[derive(PrimitiveEnum_u8, Clone, Copy, PartialEq, Debug, Default, Ord, PartialOrd, Eq)]
pub enum ValueType {
    #[default]
    None = 0x00,
    /// Bool values take up 1 bit in the input report
    Bool = 0x01,

    /// Uint8 values take up 1 byte in the input report
    UInt8 = 0x02,
    /// Uint16 values take up 2 bytes in the input report
    UInt16 = 0x03,
    /// Uint32 values take up 4 bytes in the input report
    UInt32 = 0x04,
    /// Uint64 values take up 8 bytes in the input report
    UInt64 = 0x05,
    /// Int8 values take up 1 byte in the input report
    Int8 = 0x06,
    /// Int16 values take up 2 bytes in the input report
    Int16 = 0x07,
    /// Int32 values take up 4 bytes in the input report
    Int32 = 0x08,
    /// Int64 values take up 8 bytes in the input report
    Int64 = 0x09,

    /// UInt8Vector2 values take up 2 bytes in the input report
    UInt8Vector2 = 0x12,
    /// UInt16Vector2 values take up 4 bytes in the input report
    UInt16Vector2 = 0x13,
    /// UInt32Vector2 values take up 8 bytes in the input report
    UInt32Vector2 = 0x14,
    /// UInt64Vector2 values take up 16 bytes in the input report
    UInt64Vector2 = 0x15,
    /// Int8Vector2 values take up 2 bytes in the input report
    Int8Vector2 = 0x16,
    /// Int16Vector2 values take up 4 bytes in the input report
    Int16Vector2 = 0x17,
    /// Int32Vector2 values take up 8 bytes in the input report
    Int32Vector2 = 0x18,
    /// Int64Vector2 values take up 16 bytes in the input report
    Int64Vector2 = 0x19,

    /// UInt8Vector3 values take up 3 bytes in the input report
    UInt8Vector3 = 0x22,
    /// UInt16Vector3 values take up 6 bytes in the input report
    UInt16Vector3 = 0x23,
    /// UInt32Vector3 values take up 12 bytes in the input report
    UInt32Vector3 = 0x24,
    /// UInt64Vector3 values take up 24 bytes in the input report
    UInt64Vector3 = 0x25,
    /// Int8Vector3 values take up 3 bytes in the input report
    Int8Vector3 = 0x26,
    /// Int16Vector3 values take up 6 bytes in the input report
    Int16Vector3 = 0x27,
    /// Int32Vector3 values take up 12 bytes in the input report
    Int32Vector3 = 0x28,
    /// Int64Vector3 values take up 24 bytes in the input report
    Int64Vector3 = 0x29,

    /// Touch values takes up 6 bytes in the input report
    Touch = 0x30,
}

impl ValueType {
    /// Returns the size in bits the value type takes up in the input data report
    pub fn size_bits(&self) -> usize {
        match self {
            ValueType::None => 0,
            ValueType::Bool => BoolValue::packed_bits(),
            ValueType::UInt8 => UInt8Value::packed_bits(),
            ValueType::UInt16 => UInt16Value::packed_bits(),
            ValueType::UInt32 => todo!(),
            ValueType::UInt64 => todo!(),
            ValueType::Int8 => todo!(),
            ValueType::Int16 => todo!(),
            ValueType::Int32 => todo!(),
            ValueType::Int64 => todo!(),
            ValueType::UInt8Vector2 => todo!(),
            ValueType::UInt16Vector2 => UInt16Vector2Value::packed_bits(),
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
            ValueType::Int16Vector3 => Int16Vector3Value::packed_bits(),
            ValueType::Int32Vector3 => todo!(),
            ValueType::Int64Vector3 => todo!(),
            ValueType::Touch => TouchValue::packed_bits(),
        }
    }

    /// Returns the size in bytes the value type takes up in the input data report
    /// rounded to the nearest byte
    pub fn size_bytes(&self) -> usize {
        let size_bits = self.size_bits();
        if size_bits < 8 {
            return 1;
        }
        size_bits / 8
    }

    /// Returns the sort priority of the value type to determine the order that these
    /// value types will appear in the input data report. Lower numbers should
    /// be ordered first.
    pub fn order_priority(&self) -> u8 {
        match self {
            ValueType::None => 100,
            ValueType::Bool => 99,
            ValueType::UInt8 => 98,
            ValueType::UInt16 => 97,
            ValueType::UInt32 => 96,
            ValueType::UInt64 => 95,
            ValueType::Int8 => 94,
            ValueType::Int16 => 93,
            ValueType::Int32 => 92,
            ValueType::Int64 => 91,
            ValueType::UInt8Vector2 => 90,
            ValueType::UInt16Vector2 => 89,
            ValueType::UInt32Vector2 => 88,
            ValueType::UInt64Vector2 => 87,
            ValueType::Int8Vector2 => 86,
            ValueType::Int16Vector2 => 85,
            ValueType::Int32Vector2 => 84,
            ValueType::Int64Vector2 => 83,
            ValueType::UInt8Vector3 => 82,
            ValueType::UInt16Vector3 => 81,
            ValueType::UInt32Vector3 => 80,
            ValueType::UInt64Vector3 => 79,
            ValueType::Int8Vector3 => 78,
            ValueType::Int16Vector3 => 77,
            ValueType::Int32Vector3 => 76,
            ValueType::Int64Vector3 => 75,
            ValueType::Touch => 74,
        }
    }
}
