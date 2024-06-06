use bevy::math::UVec2;
use bevy::prelude::Event;

use wfc_polygon::grid::FlatTopHexGrid;
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::hex::tile_id::HexTileId;
use crate::HexMode;

#[derive(Debug, Copy, Clone, Event)]
pub struct RegenerateMap;

#[derive(Debug, Copy, Clone, Event)]
pub struct ClearCache;

#[derive(Event)]
pub struct MapGenerated(pub WaveFunctionCollapse<FlatTopHexGrid, HexTileId>);

#[derive(Event)]
pub struct GridCellSet {
    pub tile: Option<HexTileId>,
    pub pos: UVec2,
}

#[derive(Clone, Event)]
pub struct WfcStep;

#[derive(Clone, Event)]
pub struct ChangeHexMode(pub HexMode);

#[derive(Clone, Event)]
pub struct GridCellUpdate(pub UVec2);
