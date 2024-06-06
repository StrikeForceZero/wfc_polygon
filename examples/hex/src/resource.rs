use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use bevy::utils::HashMap;
use rand::prelude::StdRng;

use wfc_polygon::wfc::WrapMode;

use crate::{AnimateMode, TextMode};
use crate::color_wrapper::ColorWrapper;
use crate::component::{HexData, HexInvalidPossibilities, HexPos, HexPossibilities};

#[derive(Debug, Resource)]
pub struct GenMapSystemId(pub SystemId);

#[derive(Debug, Default, Reflect, Resource)]
pub struct HexPossibilitiesCache(
    pub HashMap<HexPos, (HexData, HexPossibilities, HexInvalidPossibilities)>,
);

#[derive(Debug, Default, Reflect, Resource)]
pub struct HexScale(pub f32);

#[derive(Debug, Default, Resource)]
pub struct ColorMaterialMap(pub HashMap<ColorWrapper, Handle<ColorMaterial>>);

#[derive(Debug, Default, Resource)]
pub struct GridSize(pub UVec2);

#[derive(Debug, Default, Resource)]
pub struct HexTextMode(pub Option<TextMode>);

#[derive(Debug, Default, Reflect, Resource)]
pub struct WfcAnimate(pub AnimateMode);

#[derive(Debug, Default, Resource)]
pub struct WfcWrapMode(pub Option<WrapMode>);

#[derive(Debug, Default, Resource)]
pub struct Seed(pub Option<u64>);

#[derive(Debug, Default, Resource)]
pub struct CustomRng(pub Option<StdRng>);
