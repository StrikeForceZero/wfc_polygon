use bevy::prelude::*;
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_mod_picking::prelude::*;

use plugin::SubPlugin;

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
    App::new()
        .add_plugins(DefaultPlugins.set(low_latency_window_plugin()))
        .add_plugins(EguiPlugin)
        // .add_plugins(WorldInspectorPlugin::default())
        .insert_resource(DebugPickingMode::Normal)
        .add_plugins(DefaultPickingPlugins)
        .add_plugins(SubPlugin)
        .run();
}
