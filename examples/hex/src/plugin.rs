use bevy::prelude::*;
use itertools::Itertools;

use crate::{config, hex, input, ui, wfc};
use crate::config::{DEBUG_COMPATIBILITY_MAP, GridCellScale, GridSize, HEX_MODE, HexMode};
use crate::hex::segment::HexSegmentId;
use crate::hex::tile_id::HexTileId;
use crate::wfc::RegenerateMap;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        app
            /* rustfmt next line chain */
            .add_plugins(config::SubPlugin)
            .add_plugins(hex::SubPlugin)
            .add_plugins(wfc::SubPlugin)
            .add_plugins(input::SubPlugin)
            .add_plugins(ui::SubPlugin)
            .add_systems(Startup, setup)
        /* rustfmt next line semi-colon */
        ;
    }
}

#[derive(Debug, Copy, Clone, Reflect, Component, PartialEq, Eq, Hash)]
pub struct MainCamera;

pub fn setup(
    mut commands: Commands,
    hex_scale: Res<GridCellScale>,
    grid_size: Res<GridSize>,
    mut regenerate_map_event_writer: EventWriter<RegenerateMap>,
) {
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
    commands.spawn((
        MainCamera,
        Camera2dBundle {
            projection: OrthographicProjection {
                far: 1000.,
                near: -1000.,
                scale: 0.05 * hex_scale.0,
                ..default()
            },
            transform: Transform::from_translation(Vec3::new(
                grid_size.0.x as f32 * hex_scale.0 / 2.0,
                grid_size.0.y as f32 * hex_scale.0 / 2.0,
                0.0,
            )),
            ..default()
        },
    ));
    regenerate_map_event_writer.send(RegenerateMap);
}
