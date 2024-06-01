use bevy::math::UVec2;
use bevy::prelude::*;
use bevy::utils::HashSet;

use wfc_polygon::grid::FlatTopHexGrid;
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::hex::map::FlatTopHexagonalSegmentIdMap;
use crate::hex::tile_id::HexTileId;

#[derive(Default, Resource)]
pub struct HexGrid(pub Option<WaveFunctionCollapse<FlatTopHexGrid, HexTileId>>);

#[derive(Debug, Copy, Clone, Reflect, Component, PartialEq, Eq, Hash)]
pub struct HexPos(pub UVec2);

#[derive(Debug, Clone, Reflect, Component)]
pub struct HexData(pub Option<FlatTopHexagonalSegmentIdMap>);

#[derive(Debug, Clone, Reflect, Component)]
pub struct HexPossibilities(pub HashSet<HexTileId>);

#[derive(Debug, Copy, Clone, Component, Reflect)]
pub struct InnerHex;

#[derive(Debug, Default, Reflect, Component)]
pub struct HexInvalid;
