use bevy::prelude::*;
use bevy::utils::HashMap;
use rand::prelude::StdRng;

use crate::color_wrapper::ColorWrapper;

#[derive(Debug, Default, Resource)]
pub struct ColorMaterialMap(pub HashMap<ColorWrapper, Handle<ColorMaterial>>);

#[derive(Debug, Default, Resource)]
pub struct CustomRng(pub Option<StdRng>);
