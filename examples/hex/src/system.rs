use bevy::prelude::*;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::utils::{HashMap, HashSet};
use bevy_inspector_egui::bevy_egui::EguiContexts;
use bevy_inspector_egui::egui;
use bevy_mod_picking::highlight::InitialHighlight;
use bevy_mod_picking::prelude::*;

use wfc_polygon::grid::{FlatTopHexGrid, GridType};
use wfc_polygon::wfc::WaveFunctionCollapse;

use crate::*;
use crate::component::*;
use crate::resource::*;

pub(crate) fn setup(gen_map_system_id: Res<GenMapSystemId>, mut commands: Commands) {
    commands.spawn(Camera2dBundle {
        projection: OrthographicProjection {
            far: 1000.,
            near: -1000.,
            scale: 0.05 * SCALE,
            ..default()
        },
        ..default()
    });
    commands.run_system(gen_map_system_id.0);
}

pub(crate) fn gen_map(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut hex_grid: ResMut<HexGrid>,
    mut hex_possibilities_cache: ResMut<HexPossibilitiesCache>,
    hex_query: Query<Entity, With<HexData>>,
) {
    hex_possibilities_cache.0.clear();
    for entity in hex_query.iter() {
        commands.entity(entity).despawn_recursive();
    }

    let mut wfc = WaveFunctionCollapse::new_with_compatibility(
        FlatTopHexGrid::new(25, 25),
        Hex::get_compatibility_map(),
    );

    /*grid.set(
        10,
        10,
        Hex((
            HexSegment::MountainPeak,
            HexSegment::MountainPeak,
            HexSegment::MountainPeak,
            HexSegment::MountainPeak,
            HexSegment::MountainPeak,
            HexSegment::MountainPeak,
        )),
    );*/

    if MODE == Mode::Full {
        for ((_, hex, side), patterns) in Hex::get_compatibility_map().iter() {
            println!(
                "{:?} {side:?} {:?}",
                hex.0 .0,
                patterns.iter().map(|h| h.0 .0).collect::<Vec<_>>()
            );
        }
    }

    let max_retries = 100;
    for n in 1..=max_retries {
        println!("attempt {n}/{max_retries}");
        if wfc.collapse() {
            println!("collapse successful");
            break;
        }
    }

    let is_grid_valid = wfc.is_valid(false);
    println!("is valid: {is_grid_valid}");

    let invalids: HashSet<(usize, usize)> = if !is_grid_valid {
        wfc.get_invalids(false).into_iter().collect()
    } else {
        HashSet::new()
    };

    let empty_cell_count = wfc.grid().cells().iter().filter(|c| c.is_none()).count();
    println!("empty cells: {empty_cell_count}");

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
        let mut position = Vec3::new(translate_x, translate_y, 0.0) * SCALE
            - Vec2::new(16.0, 20.0).extend(0.0) * SCALE;

        if x % 2 == 0 {
            position.y += 0.9 * SCALE;
        }
        let hex_sides: Option<FlatHexSegments> = (*cell).map(|tile| tile.into());
        let mesh_color_tuples = crate::hex_mesh(hex_sides);
        let id = commands
            .spawn((
                HexData(hex_sides),
                HexPos(UVec2::new(x as u32, y as u32)),
                HexPossibilities(possibilities.iter().cloned().collect::<HashSet<Hex>>()),
                SpatialBundle::from_transform(Transform::from_translation(position)),
            ))
            .with_children(|children| {
                for (mesh, color) in mesh_color_tuples {
                    // AABBs don't get recalculated and break ray-casts / picking, so we need to either:
                    // - resize mesh via mesh.scaled_by instead of transform.scale
                    // - use transform.scale and call mesh.compute_aabb() after the mesh is loaded in the scene to fix it
                    // https://github.com/bevyengine/bevy/issues/4294
                    let mesh = mesh.scaled_by(Vec2::splat(0.95 * SCALE).extend(0.0));
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
    hex_grid.0 = Some(wfc);
}

pub(crate) fn input_handler(
    mut commands: Commands,
    mouse_input: Res<ButtonInput<MouseButton>>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    gen_map_system_id: Res<GenMapSystemId>,
) {
    if keyboard_input.pressed(KeyCode::ControlLeft) && mouse_input.just_pressed(MouseButton::Left) {
        commands.run_system(gen_map_system_id.0);
    }
}

pub(crate) fn update(
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

pub(crate) fn invalid_hex_handler(
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

pub(crate) fn ui(
    mut egui_contexts: EguiContexts,
    hex_possibilities_cache: Res<HexPossibilitiesCache>,
) {
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
