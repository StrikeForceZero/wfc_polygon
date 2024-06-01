use bevy::ecs::system::SystemId;
use bevy::prelude::{Reflect, Resource};
use bevy::utils::HashMap;

use crate::component::{HexData, HexPos, HexPossibilities};

#[derive(Debug, Resource)]
pub(crate) struct GenMapSystemId(pub(crate) SystemId);

#[derive(Debug, Default, Reflect, Resource)]
pub(crate) struct HexPossibilitiesCache(pub(crate) HashMap<HexPos, (HexData, HexPossibilities)>);
