use packed_struct::prelude::*;

use super::reports::ValueType;

/// [Value] defines all the possible values that a unified gamepad can send as
/// part of its HID reports.
#[derive(Debug, Clone, Default, PartialEq)]
pub enum Value {
    #[default]
    None,
    Bool(BoolValue),
    UInt8(UInt8Value),
    UInt16(UInt16Value),
    UInt16Vector2(UInt16Vector2Value),
    Int16Vector3(Int16Vector3Value),
    Touch(TouchValue),
}

impl Value {
    /// Return the [ValueType] for this [Value]
    #[allow(dead_code)]
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::None => ValueType::None,
            Value::Bool(_) => ValueType::Bool,
            Value::UInt8(_) => ValueType::UInt8,
            Value::UInt16(_) => ValueType::UInt16,
            Value::UInt16Vector2(_) => ValueType::UInt16Vector2,
            Value::Int16Vector3(_) => ValueType::Int16Vector3,
            Value::Touch(_) => ValueType::Touch,
        }
    }
}

/// [BoolValue] defines boolean values that can be sent in an HID report, typically
/// used for inputs like buttons or switches.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bits = "1")]
pub struct BoolValue {
    #[packed_field(bits = "0")]
    pub value: bool,
}

/// [UInt8Value] defines u8 values that can be sent in an HID report, typically
/// used for low resolution trigger inputs.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "1")]
pub struct UInt8Value {
    #[packed_field(bytes = "0")]
    pub value: u8,
}

/// [UInt16Value] defines u16 values that can be sent in an HID report, typically
/// used for higher resolution trigger inputs.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "2")]
pub struct UInt16Value {
    #[packed_field(bytes = "0..=1", endian = "lsb")]
    pub value: u16,
}

/// [UInt16Vector2Value] defines (x, y) values that can be sent in an HID report, typically
/// used for inputs which require 2-axes of positive values.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "4")]
pub struct UInt16Vector2Value {
    #[packed_field(bytes = "0..=1", endian = "lsb")]
    pub x: u16,
    #[packed_field(bytes = "2..=3", endian = "lsb")]
    pub y: u16,
}

/// [Int16Vector3Value] defines (x, y, z) values that can be sent in an HID report, typically
/// used for accelerometer, gyroscope, or magnetometer input.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "6")]
pub struct Int16Vector3Value {
    #[packed_field(bytes = "0..=1", endian = "lsb")]
    pub x: i16,
    #[packed_field(bytes = "2..=3", endian = "lsb")]
    pub y: i16,
    #[packed_field(bytes = "4..=5", endian = "lsb")]
    pub z: i16,
}

/// [TouchValue] defines touch values that can be sent in an HID report.
#[derive(PackedStruct, Debug, Copy, Clone, PartialEq)]
#[packed_struct(bit_numbering = "msb0", size_bytes = "6")]
pub struct TouchValue {
    /// The finger id of the touch input for multi-touch devices.
    #[packed_field(bits = "0..=6")]
    pub index: Integer<u8, packed_bits::Bits<7>>,
    /// Whether or not the device is sensing touch.
    #[packed_field(bits = "7")]
    pub is_touching: bool,
    /// Optionally the amount of pressure the touch is experiencing, normalized
    /// between 0 and 255.
    #[packed_field(bytes = "1")]
    pub pressure: u8,
    /// The X position of the touch, normalized between 0.0-1.0, where 0
    /// is the left side of the input device and where 1.0 is the right side
    #[packed_field(bytes = "2..=3", endian = "lsb")]
    pub x: u16,
    /// The Y position of the touch, normalized between 0.0-1.0, where 0
    /// is the top side of the input device and where 1.0 is the bottom side
    #[packed_field(bytes = "4..=5", endian = "lsb")]
    pub y: u16,
}
