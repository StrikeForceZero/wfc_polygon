use bevy::prelude::*;

use crate::{config, hex, input, ui, wfc};
use crate::config::{GridCellScale, GridSize};
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
