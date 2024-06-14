use std::fmt::Formatter;
use std::sync::RwLock;

use bevy::prelude::*;
use rand::prelude::StdRng;
use rand::rngs::mock::StepRng;
use rand::SeedableRng;

use wfc_polygon::wfc::WrapMode;

use crate::input::InputMap;
use crate::resource::CustomRng;
use crate::util::create_custom_rng;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let (custom_seed, custom_rng) = if let Some(initial_seed) = INITIAL_SEED {
            (Some(initial_seed), Some(create_custom_rng(initial_seed)))
        } else {
            (None, None)
        };
        app
            /* rustfmt next line chain */
            .insert_resource(CustomRng(custom_rng))
            .insert_resource(GridCellScale(20.0))
            .insert_resource(GridSize(UVec2::splat(40)))
            .insert_resource(GridCellDebugTextMode(None))
            .insert_resource(WfcAnimate(WfcAnimateMode::FullAuto))
            .insert_resource(WfcWrapMode(Some(WrapMode::Both)))
            .insert_resource(WfcSeed(custom_seed))
            .insert_resource(InputMap {
                // input overrides here
                ..default()
            })
            .register_type::<GridCellScale>()
            .register_type::<GridSize>()
            .register_type::<WfcAnimate>()
            .register_type::<DebugTextMode>()
            .add_event::<ChangeHexMode>()
            .add_systems(Update, change_hex_mode_event_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

#[derive(Debug, Default, Reflect, Resource)]
pub struct GridSize(pub UVec2);

#[derive(Debug, Default, Reflect, Resource)]
pub struct GridCellScale(pub f32);

#[derive(Debug, Default, Reflect, Resource)]
pub struct GridCellDebugTextMode(pub Option<DebugTextMode>);

#[derive(Debug, Default, Reflect, Resource)]
pub struct WfcAnimate(pub WfcAnimateMode);

#[derive(Debug, Default, Resource)]
pub struct WfcWrapMode(pub Option<WrapMode>);

#[derive(Debug, Default, Reflect, Resource)]
pub struct WfcSeed(pub Option<u64>);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum HexMode {
    Full,
    Segments,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Reflect)]
pub enum WfcAnimateMode {
    #[default]
    FullAuto,
    SingleAuto,
    SingleManual,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Reflect)]
pub enum DebugTextMode {
    Index,
    #[default]
    PossibilityCount,
}

#[derive(Clone, Event)]
pub struct ChangeHexMode(pub HexMode);

pub fn change_hex_mode_event_handler(mut events: EventReader<ChangeHexMode>) {
    if let Some(event) = events.read().last() {
        let new_hex_mode = event.0;
        let hex_mode = *HEX_MODE.read().unwrap();
        if hex_mode == new_hex_mode {
            return;
        }
        *HEX_MODE.write().unwrap() = new_hex_mode;
    }
}

// TODO: this is silly but due to the current way its being read in `HexTileId` it needs to remain global
pub(crate) static HEX_MODE: RwLock<HexMode> = RwLock::new(HexMode::Full);
pub const DEBUG_COMPATIBILITY_MAP: bool = true;
const INITIAL_SEED: Option<u64> = Some(813985498);
