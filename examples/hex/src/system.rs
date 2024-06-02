use bevy::prelude::*;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::utils::{HashMap, HashSet};
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::egui;
use bevy_mod_picking::highlight::InitialHighlight;
use bevy_mod_picking::prelude::*;

use wfc_polygon::grid::{FlatTopHexGrid, GridType};
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::{HEX_MODE, HexMode};
use crate::color_wrapper::ColorWrapper;
use crate::component::*;
use crate::event::{ChangeHexMode, ClearCache, MapGenerated, RegenerateMap};
use crate::hex::map::FlatTopHexagonalSegmentIdMap;
use crate::hex::tile_id::HexTileId;
use crate::resource::*;

pub fn setup(
    mut commands: Commands,
    hex_scale: Res<HexScale>,
    mut event_writer: EventWriter<RegenerateMap>,
) {
    commands.spawn(Camera2dBundle {
        projection: OrthographicProjection {
            far: 1000.,
            near: -1000.,
            scale: 0.05 * hex_scale.0,
            ..default()
        },
        ..default()
    });
    event_writer.send(RegenerateMap);
}

pub fn gen_map(mut event_writer: EventWriter<MapGenerated>) {
    println!("generating map");
    let mut wfc = WaveFunctionCollapse::new_with_compatibility(
        FlatTopHexGrid::new(25, 25),
        HexTileId::get_compatibility_map(),
    );

    let max_retries = 100;
    for n in 1..=max_retries {
        println!("collapse attempt {n}/{max_retries}");
        if wfc.collapse().unwrap_or_else(|err| panic!("{err}")) {
            println!("collapse successful");
            break;
        }
    }
    event_writer.send(MapGenerated(wfc));
}

pub fn input_handler(
    mouse_input: Res<ButtonInput<MouseButton>>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut regen_map_event_writer: EventWriter<RegenerateMap>,
    mut change_hex_mode_event_writer: EventWriter<ChangeHexMode>,
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

pub fn map_generated_event_handler(
    mut commands: Commands,
    mut events: EventReader<MapGenerated>,
    hex_query: Query<Entity, With<HexData>>,
    hex_scale: Res<HexScale>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    if let Some(MapGenerated(wfc)) = events.read().last() {
        // despawn last map
        for entity in hex_query.iter() {
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

        let mut color_materials: HashMap<ColorWrapper, Handle<ColorMaterial>> = HashMap::new();
        for (ix, (cell, possibilities)) in wfc
            .grid()
            .cells()
            .iter()
            .zip(wfc.cached_possibilities().iter())
            .enumerate()
        {
            let (x, y) = wfc.grid().index_to_xy(ix);
            let is_hex_invalid = invalids.contains(&(x, y));
            let translate_x = x as f32 * 1.5;
            let translate_y = y as f32 * 1.732;
            let mut position = Vec3::new(translate_x, translate_y, 0.0) * hex_scale.0
                - Vec2::new(16.0, 20.0).extend(0.0) * hex_scale.0;

            if x % 2 == 0 {
                position.y += 0.9 * hex_scale.0;
            }
            let hex_sides: Option<FlatTopHexagonalSegmentIdMap> = (*cell).map(|tile| tile.into());
            let mesh_color_tuples = crate::hex::mesh::hex_mesh(hex_sides);
            let id = commands
                .spawn((
                    HexData(hex_sides),
                    HexPos(UVec2::new(x as u32, y as u32)),
                    HexPossibilities(
                        possibilities
                            .iter()
                            .cloned()
                            .collect::<HashSet<HexTileId>>(),
                    ),
                    SpatialBundle::from_transform(Transform::from_translation(position)),
                ))
                .with_children(|children| {
                    for (mesh, color) in mesh_color_tuples {
                        // AABBs don't get recalculated and break ray-casts / picking, so we need to either:
                        // - resize mesh via mesh.scaled_by instead of transform.scale
                        // - use transform.scale and call mesh.compute_aabb() after the mesh is loaded in the scene to fix it
                        // https://github.com/bevyengine/bevy/issues/4294
                        let mesh = mesh.scaled_by(Vec2::splat(0.95 * hex_scale.0).extend(0.0));
                        let mesh = Mesh2dHandle(meshes.add(mesh));
                        let handle = &*color_materials
                            .entry(ColorWrapper(color))
                            .or_insert_with(|| materials.add(ColorMaterial::from(color)));
                        let material = handle.clone();
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
                        ..default()
                    });
                })
                .id();
            if is_hex_invalid {
                commands.entity(id).insert(HexInvalid);
            }
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
