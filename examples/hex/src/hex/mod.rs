use bevy::prelude::*;

use wfc_polygon::compatibility_map::CompatibilityMap;
use wfc_polygon::FlatTopHexSide;
use wfc_polygon::grid::FlatTopHexGrid;

use crate::config::HexMode;
use crate::hex::segment::HexSegmentId;
use crate::hex::tile_id::HexTileId;

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

#[derive(Resource)]
pub enum HexCompatabilityMap {
    Full(CompatibilityMap<FlatTopHexGrid, HexTileId>),
    Segments(CompatibilityMap<FlatTopHexGrid, HexTileId>),
}

impl HexCompatabilityMap {
    fn get_or_create_full(&mut self) -> &Self {
        match self {
            Self::Full(_) => self,
            Self::Segments(_) => {
                *self = Self::Full(HexTileId::get_compatibility_map());
                self
            }
        }
    }
    fn get_or_create_segments(&mut self) -> &Self {
        match self {
            Self::Full(_) => {
                *self = Self::Segments(HexTileId::get_compatibility_map());
                self
            }
            Self::Segments(_) => self,
        }
    }
    pub fn create(hex_mode: HexMode) -> Self {
        match hex_mode {
            HexMode::Full => Self::Full(HexTileId::get_compatibility_map()),
            HexMode::Segments => Self::Segments(HexTileId::get_compatibility_map()),
        }
    }
    pub fn get_or_create(&mut self, hex_mode: HexMode) -> &Self {
        match hex_mode {
            HexMode::Full => self.get_or_create_full(),
            HexMode::Segments => self.get_or_create_segments(),
        }
    }
    pub fn get_compatibility_map(&self) -> &CompatibilityMap<FlatTopHexGrid, HexTileId> {
        match self {
            HexCompatabilityMap::Full(compat_map) => &compat_map,
            HexCompatabilityMap::Segments(compat_map) => &compat_map,
        }
    }
}

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        app
            /* rustfmt next line chain */
            .register_type::<HexTileId>()
            .insert_resource(HexCompatabilityMap::create(*crate::config::HEX_MODE.read().expect("failed to read HEX_MODE")))
        /* rustfmt next line semi-colon */
        ;
    }
}
