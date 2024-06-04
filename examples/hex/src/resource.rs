use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use bevy::utils::HashMap;

use crate::color_wrapper::ColorWrapper;
use crate::component::{HexData, HexPos, HexPossibilities};

#[derive(Debug, Resource)]
pub struct GenMapSystemId(pub SystemId);

#[derive(Debug, Default, Reflect, Resource)]
pub struct HexPossibilitiesCache(pub HashMap<HexPos, (HexData, HexPossibilities)>);

#[derive(Debug, Default, Reflect, Resource)]
pub struct HexScale(pub f32);

#[derive(Debug, Default, Resource)]
pub struct ColorMaterialMap(pub HashMap<ColorWrapper, Handle<ColorMaterial>>);

#[derive(Debug, Default, Resource)]
pub struct GridSize(pub UVec2);

#[derive(Debug, Default, Resource)]
pub struct HexTextEnabled(pub bool);
