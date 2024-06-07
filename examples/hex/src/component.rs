use bevy::math::UVec2;
use bevy::prelude::*;
use bevy::utils::HashSet;

use crate::hex::map::FlatTopHexagonalSegmentIdMap;
use crate::hex::tile_id::HexTileId;

#[derive(Debug, Copy, Clone, Reflect, Component, PartialEq, Eq, Hash)]
pub struct MainCamera;

#[derive(Debug, Copy, Clone, Reflect, Component, PartialEq, Eq, Hash)]
pub struct HexPos(pub UVec2);

#[derive(Debug, Clone, Reflect, Component)]
pub struct HexData(pub Option<FlatTopHexagonalSegmentIdMap>);

#[derive(Debug, Clone, Reflect, Component)]
pub struct HexIx(pub usize);

#[derive(Debug, Clone, Reflect, Component)]
pub struct HexPossibilities(pub HashSet<HexTileId>);

#[derive(Debug, Copy, Clone, Component, Reflect)]
pub struct InnerHex;

#[derive(Debug, Default, Reflect, Component)]
pub struct HexInvalid;

#[derive(Debug, Default, Reflect, Component)]
pub struct HexText(pub String);
