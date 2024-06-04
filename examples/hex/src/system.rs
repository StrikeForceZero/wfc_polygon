use std::io::{Read, Write};

use bevy::input::mouse::{MouseScrollUnit, MouseWheel};
use bevy::prelude::*;
use bevy::prelude::KeyCode::{KeyT, KeyY};
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::utils::HashSet;
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::egui;
use bevy_mod_picking::highlight::InitialHighlight;
use bevy_mod_picking::prelude::*;

use wfc_polygon::grid::{FlatTopHexGrid, GridType};
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::{HEX_MODE, HexMode};
use crate::color_wrapper::ColorWrapper;
use crate::component::*;
use crate::event::{ChangeHexMode, ClearCache, GridCellSet, MapGenerated, RegenerateMap, WfcStep};
use crate::hex::map::FlatTopHexagonalSegmentIdMap;
use crate::hex::tile_id::HexTileId;
use crate::resource::*;

pub fn setup(
    mut commands: Commands,
    hex_scale: Res<HexScale>,
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
            ..default()
        },
    ));
    regenerate_map_event_writer.send(RegenerateMap);
}

#[derive(Default)]
pub struct GenMapLocalState {
    wfc: Option<WaveFunctionCollapse<FlatTopHexGrid, HexTileId>>,
}

pub fn gen_map(
    mut commands: Commands,
    hex_query: Query<Entity, With<HexData>>,
    mut local_state: Local<GenMapLocalState>,
    mut regenerate_map_event_writer: EventWriter<MapGenerated>,
    mut wfc_step_event_writer: EventWriter<WfcStep>,
    mut grid_cell_set_event_writer: EventWriter<GridCellSet>,
    wfc_animate: Res<WfcAnimate>,
    grid_size: Res<GridSize>,
) {
    let mut consume_wfc = false;
    if let Some(inner_wfc) = local_state.wfc.as_mut() {
        if wfc_animate.0 {
            if let Some((tile, (x, y))) = inner_wfc.step() {
                // println!("set ({x}, {y}) - {tile:?}");
                grid_cell_set_event_writer.send(GridCellSet {
                    tile,
                    pos: UVec2::from((x as u32, y as u32)),
                });
                wfc_step_event_writer.send(WfcStep);
            } else {
                consume_wfc = true;
            }
        } else {
            inner_wfc.perform_all_steps();
            consume_wfc = true
        }
    } else {
        for entity in hex_query.iter() {
            commands.entity(entity).despawn_recursive();
        }
        println!("generating map");
        let compatibility_map = HexTileId::get_compatibility_map();
        println!(
            "compatibility_map size: {:.4}Mb",
            compatibility_map.estimated_size() / 1024 / 1024
        );
        if let Err(err) = compatibility_map.check_contradictions() {
            panic!("contradictions in compat map found: {err}");
        }
        println!("initializing wfc");
        let mut wfc = WaveFunctionCollapse::new_with_compatibility(
            FlatTopHexGrid::new(grid_size.0.x as usize, grid_size.0.y as usize),
            compatibility_map,
        );

        if wfc_animate.0 {
            wfc.initialize_collapse();
            local_state.wfc = Some(wfc);
            wfc_step_event_writer.send(WfcStep);
        } else {
            wfc.collapse();
            local_state.wfc = Some(wfc);
            consume_wfc = true;
        }
    }
    if consume_wfc {
        regenerate_map_event_writer.send(MapGenerated(
            local_state
                .wfc
                .take()
                .unwrap_or_else(|| panic!("failed to consume wfc")),
        ));
    }
}

pub fn wfc_step_handler(
    mut commands: Commands,
    gen_map_system_id: Res<GenMapSystemId>,
    mut step_event_reader: EventReader<WfcStep>,
) {
    for _ in step_event_reader.read() {
        commands.run_system(gen_map_system_id.0);
    }
}

pub fn input_handler(
    mut commands: Commands,
    mouse_input: Res<ButtonInput<MouseButton>>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut regen_map_event_writer: EventWriter<RegenerateMap>,
    mut change_hex_mode_event_writer: EventWriter<ChangeHexMode>,
    mut camera_query: Query<(&mut Transform, &mut OrthographicProjection), With<MainCamera>>,
    mut scroll_evr: EventReader<MouseWheel>,
    mut hex_text_enabled: ResMut<HexTextEnabled>,
    mut wfc_animate: ResMut<WfcAnimate>,
    hex_scale: Res<HexScale>,
    hex_text_query: Query<Entity, With<HexText>>,
    time: Res<Time>,
) {
    if keyboard_input.pressed(KeyCode::ControlLeft) {
        if mouse_input.just_pressed(MouseButton::Left) {
            println!("sending regen map event");
            regen_map_event_writer.send(RegenerateMap);
        } else if mouse_input.just_pressed(MouseButton::Right) {
            let hex_mode = *HEX_MODE.read().unwrap();
            let new_hex_mode = match hex_mode {
                HexMode::Full => HexMode::Segments,
                HexMode::Segments => HexMode::Full,
            };
            println!("changing hex mode to {new_hex_mode:?} for next generation");
            change_hex_mode_event_writer.send(ChangeHexMode(new_hex_mode));
        }
    }
    if keyboard_input.just_pressed(KeyY) {
        wfc_animate.0 = !wfc_animate.0;
        println!("changing wfc animate to {}", wfc_animate.0);
    }
    if keyboard_input.just_pressed(KeyT) {
        hex_text_enabled.0 = !hex_text_enabled.0;
        let visibility = if hex_text_enabled.0 {
            Visibility::Inherited
        } else {
            Visibility::Hidden
        };
        for entity in hex_text_query.iter() {
            commands.entity(entity).insert(visibility);
        }
    }
    for ev in scroll_evr.read() {
        let delta = match ev.unit {
            MouseScrollUnit::Line => ev.y,
            MouseScrollUnit::Pixel => ev.y,
        };
        for (_, mut projection) in camera_query.iter_mut() {
            projection.scale -= delta * 0.5;
            projection.scale = projection.scale.max(0.05 * hex_scale.0);
        }
    }
    let speed = 200.0 * time.delta_seconds();
    for (mut camera_transform, _) in camera_query.iter_mut() {
        if keyboard_input.pressed(KeyCode::KeyW) {
            camera_transform.translation += Vec2::new(0.0, 1.0).extend(0.0) * speed;
        }
        if keyboard_input.pressed(KeyCode::KeyS) {
            camera_transform.translation += Vec2::new(0.0, -1.0).extend(0.0) * speed;
        }
        if keyboard_input.pressed(KeyCode::KeyD) {
            camera_transform.translation += Vec2::new(1.0, 0.0).extend(0.0) * speed;
        }
        if keyboard_input.pressed(KeyCode::KeyA) {
            camera_transform.translation += Vec2::new(-1.0, 0.0).extend(0.0) * speed;
        }
    }
}

pub fn cache_update_on_hex_selected_handler(
    mut commands: Commands,
    mut hex_possibilities_cache: ResMut<HexPossibilitiesCache>,
    hex_query: Query<(&HexPos, &HexData, &HexPossibilities, &Children)>,
    pick_query: Query<(&Parent, Ref<PickSelection>), (Changed<PickSelection>, With<InnerHex>)>,
    child_query: Query<Option<Ref<PickSelection>>>,
) {
    for (parent, selection) in pick_query.iter() {
        let Ok((pos, data, possibilities, children)) = hex_query.get(parent.get()) else {
            continue;
        };
        if !selection.is_changed() {
            continue;
        }
        if !selection.is_selected {
            hex_possibilities_cache.0.remove(pos);
            continue;
        }

        let mut child_inserted = false;
        for child in children.iter() {
            let Ok(selection) = child_query.get(*child) else {
                continue;
            };
            if selection.map_or(false, |selection| selection.is_selected) {
                continue;
            }
            commands
                .entity(*child)
                .insert(PickSelection { is_selected: true });
            if !child_inserted {
                child_inserted = true;
                println!("updated cache for {pos:?}");
                hex_possibilities_cache
                    .0
                    .entry(*pos)
                    .or_insert_with(|| (data.clone(), possibilities.clone()));
            }
        }
    }
}

pub fn invalid_hex_handler(
    hex_query: Query<(Ref<HexInvalid>, &Children), Added<HexInvalid>>,
    mut hex_segment_query: Query<
        (&mut Handle<ColorMaterial>, &InitialHighlight<ColorMaterial>),
        With<InnerHex>,
    >,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut removed_invalids: RemovedComponents<HexInvalid>,
) {
    let mut color_material_handle: Option<Handle<ColorMaterial>> = None;
    // add invalid color
    for (hex_invalid_ref, children) in hex_query.iter() {
        if hex_invalid_ref.is_added() {
            let color_material_handle = if let Some(color_material) = color_material_handle.as_ref()
            {
                color_material.to_owned()
            } else {
                let new_color_material_handle = materials.add(ColorMaterial::from(Color::RED));
                color_material_handle = Some(new_color_material_handle.to_owned());
                new_color_material_handle
            };
            for child in children.iter() {
                let Some((mut asset, _initial)) = hex_segment_query.get_mut(*child).ok() else {
                    continue;
                };
                color_material_handle.clone_into(&mut asset);
            }
        }
    }
    // reset color
    for removed in removed_invalids.read() {
        let Ok((_, children)) = hex_query.get(removed) else {
            continue;
        };
        for child in children.iter() {
            let Some((mut asset, initial)) = hex_segment_query.get_mut(*child).ok() else {
                continue;
            };
            initial.initial.clone_into(&mut asset);
        }
    }
}

pub fn ui(mut egui_contexts: EguiContexts, hex_possibilities_cache: Res<HexPossibilitiesCache>) {
    egui::Window::new("Possibilities").show(egui_contexts.ctx_mut(), |ui| {
        egui::ScrollArea::vertical()
            .auto_shrink([false; 2])
            .show(ui, |ui| {
                ui.heading("Click tile to view possibilities");
                for (pos, (data, possibilities)) in hex_possibilities_cache.0.iter() {
                    ui.add(egui::Label::new(format!(
                        "pos: {}\ndata: {data:#?}\npossibilities:{:#?}",
                        pos.0, possibilities.0
                    )));
                }
            });
    });
}

pub fn regen_map_event_handler(
    mut commands: Commands,
    gen_map_system_id: Res<GenMapSystemId>,
    mut events: EventReader<RegenerateMap>,
) {
    if events.read().next().is_some() {
        events.clear();
        commands.run_system(gen_map_system_id.0);
    }
}

pub fn clear_cache_event_handler(
    mut events: EventReader<ClearCache>,
    mut hex_possibilities_cache: ResMut<HexPossibilitiesCache>,
) {
    if events.read().next().is_some() {
        events.clear();
        hex_possibilities_cache.0.clear();
    }
}

pub struct CreateHexOptions {
    tile: Option<HexTileId>,
    ix: usize,
    pos: UVec2,
    possibilities: HashSet<HexTileId>,
    is_invalid: bool,
}

fn create_hex(
    create_hex_options: CreateHexOptions,
    hex_scale: &Res<HexScale>,
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    color_material_map: &mut ResMut<ColorMaterialMap>,
    hex_text_enabled: &Res<HexTextEnabled>,
) {
    let possibilities = create_hex_options.possibilities.clone();
    let scale = hex_scale.0;
    let tile = &create_hex_options.tile.clone();
    let pos = create_hex_options.pos;
    let ix = create_hex_options.ix;
    let x = pos.x;
    let y = pos.y;
    let translate_x = x as f32 * 1.5;
    let translate_y = y as f32 * 1.732;
    let mut position = Vec3::new(translate_x, translate_y, 0.0) * scale
        - Vec2::new(16.0, 20.0).extend(0.0) * scale;

    let visibility = if hex_text_enabled.0 {
        Visibility::Inherited
    } else {
        Visibility::Hidden
    };

    if x % 2 == 0 {
        position.y += 0.9 * scale;
    }
    let hex_sides: Option<FlatTopHexagonalSegmentIdMap> = tile.map(|tile| tile.into());
    let mesh_color_tuples = crate::hex::mesh::hex_mesh(hex_sides);
    let id = commands
        .spawn((
            HexData(hex_sides),
            HexPos(UVec2::new(x, y)),
            HexPossibilities(possibilities),
            SpatialBundle::from_transform(Transform::from_translation(position)),
        ))
        .with_children(|children| {
            for (mesh, color) in mesh_color_tuples {
                // AABBs don't get recalculated and break ray-casts / picking, so we need to either:
                // - resize mesh via mesh.scaled_by instead of transform.scale
                // - use transform.scale and call mesh.compute_aabb() after the mesh is loaded in the scene to fix it
                // https://github.com/bevyengine/bevy/issues/4294
                let mesh = mesh.scaled_by(Vec2::splat(0.95 * scale).extend(0.0));
                let mesh = Mesh2dHandle(meshes.add(mesh));
                let handle = &color_material_map
                    .0
                    .entry(ColorWrapper(color))
                    .or_insert_with(|| materials.add(ColorMaterial::from(color)));
                let material = (**handle).clone();
                children.spawn((
                    InnerHex,
                    MaterialMesh2dBundle {
                        mesh,
                        material,
                        ..default()
                    },
                    PickableBundle::default(),
                ));
            }
            children.spawn(Text2dBundle {
                text: Text::from_section(
                    ix.to_string(),
                    TextStyle {
                        font_size: 15.0,
                        color: Color::PURPLE,
                        ..default()
                    },
                ),
                transform: Transform::from_translation(Vec2::default().extend(10.0)),
                visibility,
                ..default()
            });
        })
        .id();
    if create_hex_options.is_invalid {
        commands.entity(id).insert(HexInvalid);
    }
}

pub fn grid_cell_set_event_handler(
    mut grid_cell_set: EventReader<GridCellSet>,
    grid_size: Res<GridSize>,
    hex_scale: Res<HexScale>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut color_material_map: ResMut<ColorMaterialMap>,
    hex_text_enabled: Res<HexTextEnabled>,
    hex_query: Query<(Entity, &HexPos)>,
) {
    for gcs in grid_cell_set.read() {
        if gcs.tile.is_none() {
            for (entity, pos) in hex_query.iter() {
                if pos.0 == gcs.pos {
                    commands.entity(entity).despawn_recursive();
                }
            }
        } else {
            create_hex(
                CreateHexOptions {
                    possibilities: HashSet::new(),
                    tile: gcs.tile,
                    ix: gcs.pos.x as usize * grid_size.0.x as usize + gcs.pos.y as usize,
                    pos: gcs.pos,
                    is_invalid: false,
                },
                &hex_scale,
                &mut commands,
                &mut meshes,
                &mut materials,
                &mut color_material_map,
                &hex_text_enabled,
            );
        }
    }
}

pub fn map_generated_event_handler(
    mut events: EventReader<MapGenerated>,
    hex_query: Query<(Entity, &HexPos), With<HexData>>,
    hex_scale: Res<HexScale>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut color_material_map: ResMut<ColorMaterialMap>,
    hex_text_enabled: Res<HexTextEnabled>,
) {
    if let Some(MapGenerated(wfc)) = events.read().last() {
        // despawn last map
        for (entity, _) in hex_query.iter() {
            commands.entity(entity).despawn_recursive();
        }

        let is_grid_valid = wfc.is_valid(false);
        println!("is valid: {is_grid_valid}");

        let empty_cell_count = wfc.grid().cells().iter().filter(|c| c.is_none()).count();
        println!("empty cells: {empty_cell_count}");

        let invalids: HashSet<(usize, usize)> = if !is_grid_valid {
            wfc.get_invalids(false).into_iter().collect()
        } else {
            HashSet::new()
        };

        for (ix, (cell, possibilities)) in wfc
            .grid()
            .cells()
            .iter()
            .zip(wfc.cached_possibilities().iter())
            .enumerate()
        {
            let (x, y) = wfc.grid().index_to_xy(ix);
            let is_hex_invalid = invalids.contains(&(x, y));
            create_hex(
                CreateHexOptions {
                    possibilities: HashSet::new(),
                    tile: *cell,
                    ix,
                    pos: UVec2::new(x as u32, y as u32),
                    is_invalid: is_hex_invalid,
                },
                &hex_scale,
                &mut commands,
                &mut meshes,
                &mut materials,
                &mut color_material_map,
                &hex_text_enabled,
            );
        }
    }
}

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
