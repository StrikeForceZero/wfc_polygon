use bevy::prelude::Event;

use wfc_polygon::grid::FlatTopHexGrid;
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::hex::tile_id::HexTileId;

#[derive(Debug, Copy, Clone, Event)]
pub struct RegenerateMap;

#[derive(Debug, Copy, Clone, Event)]
pub struct ClearCache;

#[derive(Clone, Event)]
pub struct MapGenerated(pub WaveFunctionCollapse<FlatTopHexGrid, HexTileId>);
