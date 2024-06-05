use std::fmt::Formatter;
use std::sync::RwLock;

use bevy::prelude::*;
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_mod_picking::prelude::*;

use plugin::SubPlugin;

use crate::hex::tile_id::HexTileId;

mod color_wrapper;
mod component;
mod event;
mod hex;
mod plugin;
mod resource;
mod system;
mod util;

#[derive(Debug, Copy, Clone, PartialEq)]
enum HexMode {
    Full,
    Segments,
}

impl std::fmt::Display for HexMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            HexMode::Full => "full",
            HexMode::Segments => "segments",
        };
        write!(f, "{str}")
    }
}

// TODO: this is silly but due to the current way its being read in `HexTileId` it needs to remain global
static HEX_MODE: RwLock<HexMode> = RwLock::new(HexMode::Full);
const DEBUG_COMPATIBILITY_MAP: bool = false;
const INITIAL_SEED: Option<u64> = Some(156863044);

//noinspection RsConstantConditionIf
fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(low_latency_window_plugin()))
        .add_plugins(EguiPlugin)
        // .add_plugins(WorldInspectorPlugin::default())
        .insert_resource(DebugPickingMode::Normal)
        .add_plugins(DefaultPickingPlugins)
        .add_plugins(SubPlugin)
        .run();

    if DEBUG_COMPATIBILITY_MAP {
        let mode = *HEX_MODE.read().unwrap();
        match mode {
            HexMode::Full => {
                for ((_, hex, side), patterns) in HexTileId::get_compatibility_map().iter() {
                    println!(
                        "{:?} {side:?} {:?}",
                        hex.0 .0,
                        patterns.iter().map(|h| h.0 .0).collect::<Vec<_>>()
                    );
                }
            }
            HexMode::Segments => {
                // too many to print
            }
        }
    }
}
