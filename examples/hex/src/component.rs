use bevy::math::UVec2;
use bevy::prelude::{Component, Reflect, Resource};
use bevy::utils::HashSet;

use wfc_polygon::grid::FlatTopHexGrid;
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::{FlatHexSegments, Hex};

#[derive(Default, Resource)]
pub(crate) struct HexGrid(pub(crate) Option<WaveFunctionCollapse<FlatTopHexGrid, Hex>>);

#[derive(Debug, Copy, Clone, Reflect, Component, PartialEq, Eq, Hash)]
pub(crate) struct HexPos(pub(crate) UVec2);

#[derive(Debug, Clone, Reflect, Component)]
pub(crate) struct HexData(pub(crate) Option<FlatHexSegments>);

#[derive(Debug, Clone, Reflect, Component)]
pub(crate) struct HexPossibilities(pub(crate) HashSet<Hex>);

#[derive(Debug, Copy, Clone, Component, Reflect)]
pub(crate) struct InnerHex;

#[derive(Debug, Default, Reflect, Component)]
pub(crate) struct HexInvalid;