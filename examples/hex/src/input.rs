use bevy::ecs::system::SystemParam;
use bevy::input::mouse::{MouseScrollUnit, MouseWheel};
use bevy::prelude::*;
use rand::{Rng, SeedableRng, thread_rng};
use rand::prelude::StdRng;
use rand::rngs::mock::StepRng;
use serde::{Deserialize, Serialize};

use wfc_polygon::wfc::WrapMode;

use crate::config::{
    ChangeHexMode, DebugTextMode, GridCellDebugTextMode, GridCellScale, HexMode, WfcAnimate,
    WfcAnimateMode, WfcSeed, WfcWrapMode,
};
use crate::config::HEX_MODE;
use crate::hex::tile_id::HexTileId;
use crate::plugin::MainCamera;
use crate::resource::CustomRng;
use crate::ui::UiState;
use crate::wfc::{
    GridCellDebugText, GridCellIndex, GridCellTile, RegenerateMap, WfcCellPossibilities, WfcStep,
};

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        app
            /* rustfmt next line chain */
            .init_resource::<InputMap>()
            .add_systems(Update, input_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

#[derive(Debug, Copy, Clone, Reflect, Resource, Serialize, Deserialize)]
pub struct InputMap {
    pub roll_seed: KeyCode,
    pub redraw: KeyCode,
    pub toggle_auto: KeyCode,
    pub toggle_text: KeyCode,
    pub toggle_variant: KeyCode,
    pub toggle_animate: KeyCode,
    pub toggle_wrap: KeyCode,
    pub step: KeyCode,
    pub move_camera_left: KeyCode,
    pub move_camera_right: KeyCode,
    pub move_camera_up: KeyCode,
    pub move_camera_down: KeyCode,
    pub speed_modifier: KeyCode,
    pub toggle_ui_config: KeyCode,
}

impl Default for InputMap {
    fn default() -> Self {
        Self {
            roll_seed: KeyCode::Tab,
            redraw: KeyCode::KeyR,
            toggle_auto: KeyCode::KeyP,
            toggle_text: KeyCode::KeyT,
            toggle_variant: KeyCode::KeyV,
            toggle_animate: KeyCode::KeyY,
            toggle_wrap: KeyCode::KeyM,
            step: KeyCode::Space,
            move_camera_left: KeyCode::KeyA,
            move_camera_right: KeyCode::KeyD,
            move_camera_up: KeyCode::KeyW,
            move_camera_down: KeyCode::KeyS,
            speed_modifier: KeyCode::ShiftLeft,
            toggle_ui_config: KeyCode::Escape,
        }
    }
}

#[derive(SystemParam)]
pub struct InputHandlerEvents<'w, 's> {
    scroll_evr: EventReader<'w, 's, MouseWheel>,
    regen_map_event_writer: EventWriter<'w, RegenerateMap>,
    change_hex_mode_event_writer: EventWriter<'w, ChangeHexMode>,
    wfc_step_event_writer: EventWriter<'w, WfcStep>,
}

#[derive(SystemParam)]
pub struct InputHandlerResources<'w> {
    hex_text_mode: ResMut<'w, GridCellDebugTextMode>,
    wfc_animate: ResMut<'w, WfcAnimate>,
    wfc_wrap_mode: ResMut<'w, WfcWrapMode>,
    seed_res: ResMut<'w, WfcSeed>,
    custom_rng: ResMut<'w, CustomRng>,
    ui_state: ResMut<'w, UiState>,
    input_map: Res<'w, InputMap>,
    keyboard_input: Res<'w, ButtonInput<KeyCode>>,
    hex_scale: Res<'w, GridCellScale>,
    time: Res<'w, Time>,
}

#[derive(SystemParam)]
pub struct InputHandlerQuery<'w, 's> {
    camera_query: Query<
        'w,
        's,
        (&'static mut Transform, &'static mut OrthographicProjection),
        With<MainCamera>,
    >,
    hex_query: Query<
        'w,
        's,
        (
            Entity,
            Option<&'static GridCellIndex>,
            Option<&'static WfcCellPossibilities<HexTileId>>,
        ),
        With<GridCellTile<HexTileId>>,
    >,
    hex_text_query: Query<'w, 's, (Entity, &'static Parent), With<GridCellDebugText>>,
}

#[derive(Debug, Default)]
pub struct InputHandlerLocalState {
    time_since_last_step: f32,
}

// Define all the extracted functions first
fn handle_toggle_ui_config(resources: &mut InputHandlerResources) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.toggle_ui_config)
    {
        resources.ui_state.show_config = !resources.ui_state.show_config;
    }
}

fn handle_redraw(resources: &mut InputHandlerResources, events: &mut InputHandlerEvents) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.redraw)
    {
        debug!("sending regen map event");
        events.regen_map_event_writer.send(RegenerateMap);
    }
}

fn handle_toggle_variant(resources: &mut InputHandlerResources, events: &mut InputHandlerEvents) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.toggle_variant)
    {
        let hex_mode = *HEX_MODE.read().unwrap();
        let new_hex_mode = match hex_mode {
            HexMode::Full => HexMode::Segments,
            HexMode::Segments => HexMode::Full,
        };
        info!("changing hex mode to {new_hex_mode:?} for next generation");
        events
            .change_hex_mode_event_writer
            .send(ChangeHexMode(new_hex_mode));
    }
}

fn handle_step(
    state: &mut InputHandlerLocalState,
    resources: &mut InputHandlerResources,
    events: &mut InputHandlerEvents,
) {
    let step_speed = if resources
        .keyboard_input
        .pressed(resources.input_map.speed_modifier)
    {
        0.0
    } else {
        0.15
    };
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.step)
        || resources.keyboard_input.pressed(resources.input_map.step)
            && resources.time.elapsed_seconds() - state.time_since_last_step > step_speed
    {
        state.time_since_last_step = resources.time.elapsed_seconds();
        debug!("stepping");
        events.wfc_step_event_writer.send(WfcStep);
    }
}

fn handle_toggle_animate(resources: &mut InputHandlerResources) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.toggle_animate)
    {
        resources.wfc_animate.0 = match resources.wfc_animate.0 {
            WfcAnimateMode::FullAuto => WfcAnimateMode::SingleAuto,
            WfcAnimateMode::SingleAuto => WfcAnimateMode::FullAuto,
            WfcAnimateMode::SingleManual => WfcAnimateMode::FullAuto,
        };
        info!("changing wfc animate to {:?}", resources.wfc_animate.0);
    }
}

fn handle_toggle_auto(resources: &mut InputHandlerResources) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.toggle_auto)
    {
        resources.wfc_animate.0 = match resources.wfc_animate.0 {
            WfcAnimateMode::FullAuto => WfcAnimateMode::SingleManual,
            WfcAnimateMode::SingleAuto => WfcAnimateMode::SingleManual,
            WfcAnimateMode::SingleManual => WfcAnimateMode::SingleAuto,
        };
        info!("changing wfc animate to {:?}", resources.wfc_animate.0);
    }
}

fn handle_toggle_text(
    resources: &mut InputHandlerResources,
    query: &InputHandlerQuery,
    commands: &mut Commands,
) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.toggle_text)
    {
        resources.hex_text_mode.0 = match resources.hex_text_mode.0 {
            None => Some(DebugTextMode::PossibilityCount),
            Some(DebugTextMode::PossibilityCount) => Some(DebugTextMode::Index),
            Some(DebugTextMode::Index) => None,
        };
        info!("changing text mode to {:?}", resources.hex_text_mode.0);
        for (entity, parent) in query.hex_text_query.iter() {
            let Ok((_, ix_opt, possibilities_opt)) = query.hex_query.get(parent.get()) else {
                continue;
            };
            let (visibility, text) = match resources.hex_text_mode.0 {
                None => (Visibility::Hidden, String::new()),
                Some(DebugTextMode::PossibilityCount) => (
                    Visibility::Inherited,
                    possibilities_opt
                        .map(|p| p.0.len().to_string())
                        .unwrap_or_default(),
                ),
                Some(DebugTextMode::Index) => (
                    Visibility::Inherited,
                    ix_opt.map(|ix| ix.0.to_string()).unwrap_or_default(),
                ),
            };
            commands
                .entity(entity)
                .insert((GridCellDebugText(text), visibility));
        }
    }
}

fn handle_roll_seed(resources: &mut InputHandlerResources, events: &mut InputHandlerEvents) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.roll_seed)
    {
        let seed = thread_rng().gen_range(0..1_000_000_000);
        resources.seed_res.0 = Some(seed);
        info!("seed: {seed}");
        resources.custom_rng.0 = Some(
            StdRng::from_rng(StepRng::new(seed, 1))
                .unwrap_or_else(|err| panic!("failed to create rng from thread_rng - {err}")),
        );
        debug!("sending regen map event");
        events.regen_map_event_writer.send(RegenerateMap);
    }
}

fn handle_toggle_wrap(resources: &mut InputHandlerResources) {
    if resources
        .keyboard_input
        .just_pressed(resources.input_map.toggle_wrap)
    {
        resources.wfc_wrap_mode.0 = match resources.wfc_wrap_mode.0 {
            None => Some(WrapMode::Both),
            Some(WrapMode::Both) => Some(WrapMode::X),
            Some(WrapMode::X) => Some(WrapMode::Y),
            Some(WrapMode::Y) => None,
        };
        info!("set wrap mode to {:?}", resources.wfc_wrap_mode.0);
    }
}

fn handle_scroll(
    scroll_evr: &mut EventReader<MouseWheel>,
    query: &mut InputHandlerQuery,
    resources: &mut InputHandlerResources,
) {
    for ev in scroll_evr.read() {
        let delta = match ev.unit {
            MouseScrollUnit::Line => ev.y,
            MouseScrollUnit::Pixel => ev.y,
        };
        for (_, mut projection) in query.camera_query.iter_mut() {
            projection.scale -= delta * 0.25;
            projection.scale = projection.scale.max(0.01 * resources.hex_scale.0);
        }
    }
}

fn handle_camera_movement(query: &mut InputHandlerQuery, resources: &mut InputHandlerResources) {
    let speed = if resources
        .keyboard_input
        .pressed(resources.input_map.speed_modifier)
    {
        1000.0
    } else {
        200.0
    } * resources.time.delta_seconds();
    for (mut camera_transform, _) in query.camera_query.iter_mut() {
        if resources
            .keyboard_input
            .pressed(resources.input_map.move_camera_up)
        {
            camera_transform.translation += Vec2::new(0.0, 1.0).extend(0.0) * speed;
        }
        if resources
            .keyboard_input
            .pressed(resources.input_map.move_camera_down)
        {
            camera_transform.translation += Vec2::new(0.0, -1.0).extend(0.0) * speed;
        }
        if resources
            .keyboard_input
            .pressed(resources.input_map.move_camera_right)
        {
            camera_transform.translation += Vec2::new(1.0, 0.0).extend(0.0) * speed;
        }
        if resources
            .keyboard_input
            .pressed(resources.input_map.move_camera_left)
        {
            camera_transform.translation += Vec2::new(-1.0, 0.0).extend(0.0) * speed;
        }
    }
}

// Main input handler function
pub fn input_handler(
    mut state: Local<InputHandlerLocalState>,
    mut commands: Commands,
    mut scroll_evr: EventReader<MouseWheel>,
    mut resources: InputHandlerResources,
    mut events: InputHandlerEvents,
    mut query: InputHandlerQuery,
) {
    handle_toggle_ui_config(&mut resources);
    handle_redraw(&mut resources, &mut events);
    handle_toggle_variant(&mut resources, &mut events);
    handle_step(&mut state, &mut resources, &mut events);
    handle_toggle_animate(&mut resources);
    handle_toggle_auto(&mut resources);
    handle_toggle_text(&mut resources, &query, &mut commands);
    handle_roll_seed(&mut resources, &mut events);
    handle_toggle_wrap(&mut resources);
    handle_scroll(&mut scroll_evr, &mut query, &mut resources);
    handle_camera_movement(&mut query, &mut resources);
}
