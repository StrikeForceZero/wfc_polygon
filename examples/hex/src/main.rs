use bevy::prelude::*;
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_mod_picking::prelude::*;

use plugin::SubPlugin;

mod color_wrapper;
mod component;
mod hex;
mod plugin;
mod resource;
mod system;
mod util;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Mode {
    Full,
    Segments,
}

const MODE: Mode = Mode::Full;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(low_latency_window_plugin()))
        .add_plugins(EguiPlugin)
        .add_plugins(WorldInspectorPlugin::default())
        .insert_resource(DebugPickingMode::Normal)
        .add_plugins(DefaultPickingPlugins)
        .add_plugins(SubPlugin)
        .run();
}
