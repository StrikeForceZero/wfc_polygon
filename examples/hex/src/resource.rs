use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use bevy::utils::HashMap;

use crate::component::{HexData, HexPos, HexPossibilities};

#[derive(Debug, Resource)]
pub struct GenMapSystemId(pub SystemId);

#[derive(Debug, Default, Reflect, Resource)]
pub struct HexPossibilitiesCache(pub HashMap<HexPos, (HexData, HexPossibilities)>);
