use wfc_polygon::FlatTopHexSide;

use crate::hex::segment::HexSegmentId;

pub mod map;
pub mod mesh;
pub mod segment;
pub mod tile_id;

pub const FLAT_HEX_SIDES: [FlatTopHexSide; 6] = [
    FlatTopHexSide::Top,
    FlatTopHexSide::TopRight,
    FlatTopHexSide::BottomRight,
    FlatTopHexSide::Bottom,
    FlatTopHexSide::BottomLeft,
    FlatTopHexSide::TopLeft,
];

pub type HexSegmentIdTuple = (
    HexSegmentId,
    HexSegmentId,
    HexSegmentId,
    HexSegmentId,
    HexSegmentId,
    HexSegmentId,
);
