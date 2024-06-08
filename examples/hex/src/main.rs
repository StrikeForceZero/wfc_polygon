use bevy::prelude::*;
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_mod_picking::prelude::*;
use itertools::Itertools;

use config::{DEBUG_COMPATIBILITY_MAP, HEX_MODE, HexMode};
use plugin::SubPlugin;

use crate::hex::segment::HexSegmentId;
use crate::hex::tile_id::HexTileId;

mod color_wrapper;
mod config;
mod hex;
mod input;
mod plugin;
mod resource;
mod ui;
mod util;
mod wfc;

//noinspection RsConstantConditionIf
fn main() {
    util::tracing::init_tracing();
    if DEBUG_COMPATIBILITY_MAP {
        let mode = *HEX_MODE.read().unwrap();
        let compat_map = HexTileId::get_compatibility_map();
        info!(
            "CompatibilityMap:\nSize: {} Bytes\nEntries: {}",
            compat_map.mem_size(),
            compat_map.iter().count()
        );
        match mode {
            HexMode::Full => {
                for ((_, hex, side), patterns) in compat_map.iter() {
                    fn get_segment(hex: &HexTileId) -> &HexSegmentId {
                        let HexTileId((a, b, c, d, e, f)) = hex;
                        [a, b, c, d, e, f]
                            .iter()
                            .all_equal_value()
                            .unwrap_or_else(|diff| {
                                panic!("HexMode::Full expects each segment to be the same {diff:?}")
                            })
                    }
                    let segment = get_segment(hex);
                    let valid_set = patterns.iter().map(get_segment).collect::<Vec<_>>();
                    debug!("{segment:?} {side:?} {valid_set:?}",);
                }
            }
            HexMode::Segments => {
                // too many to print
            }
        }
        if let Err(err) = compat_map.check_contradictions() {
            warn!("{err:?}");
        } else {
            info!("no contradictions");
        }
    }
    App::new()
        .add_plugins(DefaultPlugins.set(low_latency_window_plugin()))
        .add_plugins(EguiPlugin)
        // .add_plugins(WorldInspectorPlugin::default())
        .insert_resource(DebugPickingMode::Normal)
        .add_plugins(DefaultPickingPlugins)
        .add_plugins(SubPlugin)
        .run();
}
