use std::hash::Hash;
use std::marker::PhantomData;

use bevy::ecs::system::{SystemId, SystemParam};
use bevy::math::UVec2;
use bevy::prelude::*;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::utils::{HashMap, HashSet};
use bevy::utils::hashbrown::hash_map::Entry;
use bevy_mod_picking::highlight::InitialHighlight;
use bevy_mod_picking::PickableBundle;
use bevy_mod_picking::prelude::PickSelection;
use rand::{SeedableRng, thread_rng};
use rand::prelude::StdRng;
use rand::rngs::mock::StepRng;

use wfc_polygon::grid::{FlatTopHexGrid, GridType};
use wfc_polygon::Tile;
use wfc_polygon::wfc::{StepResult, WaveFunctionCollapse};

use crate::color_wrapper::ColorWrapper;
use crate::config::{
    DebugTextMode, GridCellDebugTextMode, GridCellScale, GridSize, HEX_MODE, HexMode,
    WfcAnimate, WfcAnimateMode, WfcSeed, WfcWrapMode,
};
use crate::hex::HexCompatabilityMap;
use crate::hex::map::FlatTopHexagonalSegmentIdMap;
use crate::hex::tile_id::HexTileId;
use crate::resource::{ColorMaterialMap, CustomRng};
use crate::wfc;

pub struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(gen_map));
        app
            /* rustfmt next line chain */
            .insert_resource(gen_map_res)
            .init_resource::<GridCellMeshHandle>()
            .init_resource::<GridCellDataCache<HexTileId>>()
            .init_resource::<SelectedGridCellData<HexTileId>>()
            .init_resource::<CustomRng>()
            .init_resource::<ColorMaterialMap>()
            .register_type::<GridCellPos>()
            .register_type::<GridCellTile<HexTileId>>()
            .register_type::<GridCellIndex>()
            .register_type::<WfcCellPossibilities<HexTileId>>()
            .register_type::<GridCellChild>()
            .register_type::<WfcGridCellInvalid>()
            .register_type::<GridCellDebugText>()
            .register_type::<GridCellDataCache<HexTileId>>()
            .register_type::<SelectedGridCellData<HexTileId>>()
            .add_event::<RegenerateMap>()
            .add_event::<ClearSelectedHex>()
            .add_event::<WfcStep>()
            .add_event::<MapGenerated<FlatTopHexGrid, HexTileId>>()
            .add_event::<GridCellSet<HexTileId>>()
            .add_event::<GridCellRefresh>()
            .add_systems(Update, wfc_step_handler)
            .add_systems(Update,
                         (
                             on_map_generated_event_handler,
                             on_cell_selected_handler,
                             on_clear_selection_handler,
                         ).chain(),
            )
            .add_systems(Update,
                         (
                             grid_cell_refresh,
                             grid_cell_set_event_handler,
                             on_debug_text_added,
                         ).chain(),
            )
            .add_systems(Update, regen_map_event_handler)
            .add_systems(PostUpdate, invalid_hex_handler)
        /* rustfmt next line semi-colon */
        ;
    }
}

#[derive(Default)]
pub struct GenMapLocalState {
    wfc: Option<WaveFunctionCollapse<FlatTopHexGrid, HexTileId>>,
}

#[derive(SystemParam)]
struct GenMapEvents<'w> {
    regenerate_map: EventWriter<'w, MapGenerated<FlatTopHexGrid, HexTileId>>,
    wfc_step: EventWriter<'w, WfcStep>,
    grid_cell_set: EventWriter<'w, GridCellSet<HexTileId>>,
    grid_cell_update: EventWriter<'w, GridCellRefresh>,
}

#[derive(SystemParam)]
struct GenMapResources<'w> {
    grid_size: Res<'w, GridSize>,
    wrap_mode: Res<'w, WfcWrapMode>,
    custom_rng: ResMut<'w, CustomRng>,
    possibility_cache: ResMut<'w, GridCellDataCache<HexTileId>>,
    hex_compatability_map: ResMut<'w, HexCompatabilityMap>,
    wfc_animate: ResMut<'w, WfcAnimate>,
}

pub fn gen_map(
    mut local_state: Local<GenMapLocalState>,
    mut commands: Commands,
    hex_query: Query<Entity, With<GridCellTile<HexTileId>>>,
    mut resources: GenMapResources,
    mut events: GenMapEvents,
) {
    let rng = if let Some(custom_rng) = resources.custom_rng.0.as_mut() {
        custom_rng
    } else {
        // TODO: works in nightly
        /*&mut StdRng::from_rng(thread_rng())
        .unwrap_or_else(|err| panic!("failed to create rng from thread_rng - {err}"));*/
        static mut TEMP_RNG: Option<StdRng> = None;

        unsafe {
            if TEMP_RNG.is_none() {
                TEMP_RNG =
                    Some(StdRng::from_rng(thread_rng()).unwrap_or_else(|err| {
                        panic!("failed to create rng from thread_rng - {err}")
                    }));
            }
            TEMP_RNG.as_mut().unwrap()
        }
    };
    let mut consume_wfc = false;
    if let Some(inner_wfc) = local_state.wfc.as_mut() {
        if matches!(
            resources.wfc_animate.0,
            WfcAnimateMode::SingleAuto | WfcAnimateMode::SingleManual
        ) {
            if let Some(StepResult {
                tile,
                pos: (x, y),
                unsets,
            }) = inner_wfc.step_with_custom_rng(rng)
            {
                debug!("set ({x}, {y}) - {tile:?}");
                // update all each step for easier visualization
                for (ix, tile) in inner_wfc.grid().cells().iter().enumerate() {
                    let (x, y) = inner_wfc.grid().index_to_xy(ix);
                    let data = (
                        GridCellTile(tile.map(|tile| tile.into())),
                        WfcCellPossibilities(
                            inner_wfc
                                .cached_possibilities()
                                .get(ix)
                                .cloned()
                                .unwrap_or_default()
                                .into_iter()
                                .collect(),
                        ),
                    );
                    let pos = UVec2::new(x as u32, y as u32);
                    match resources.possibility_cache.0.entry(GridCellPos(pos)) {
                        Entry::Occupied(mut entry) => {
                            entry.insert(data);
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(data);
                        }
                    }
                    events.grid_cell_update.send(GridCellRefresh(pos));
                }
                for (x, y) in unsets.unwrap_or_default() {
                    events.grid_cell_set.send(GridCellSet {
                        tile: None,
                        pos: UVec2::from((x as u32, y as u32)),
                    });
                }
                events.grid_cell_set.send(GridCellSet {
                    tile,
                    pos: UVec2::from((x as u32, y as u32)),
                });
                if resources.wfc_animate.0 == WfcAnimateMode::SingleAuto {
                    events.wfc_step.send(WfcStep);
                }
            } else {
                consume_wfc = true;
            }
        } else {
            inner_wfc.perform_all_steps_with_custom_rng(rng);
            consume_wfc = true
        }
    } else {
        for entity in hex_query.iter() {
            commands.entity(entity).despawn_recursive();
        }
        info!("generating map");
        let compatibility_map = resources
            .hex_compatability_map
            .get_or_create(*HEX_MODE.read().expect("failed to read HEX_MODE"))
            .get_compatibility_map();
        debug!("initializing wfc");
        let mut wfc = WaveFunctionCollapse::new_with_compatibility(
            FlatTopHexGrid::new(
                resources.grid_size.0.x as usize,
                resources.grid_size.0.y as usize,
            ),
            compatibility_map.clone(),
            resources.wrap_mode.0,
        );

        if matches!(
            resources.wfc_animate.0,
            WfcAnimateMode::SingleAuto | WfcAnimateMode::SingleManual
        ) {
            wfc.initialize_collapse();
            local_state.wfc = Some(wfc);
            if resources.wfc_animate.0 == WfcAnimateMode::SingleAuto {
                events.wfc_step.send(WfcStep);
            }
        } else {
            wfc.collapse_with_custom_rng(rng);
            local_state.wfc = Some(wfc);
            consume_wfc = true;
        }
    }
    if consume_wfc {
        if matches!(*HEX_MODE.read().unwrap(), HexMode::Full) {
            let grid = local_state.wfc.as_ref().unwrap().grid();
            let total_set = grid.set_count();
            if let Some(dist_map) = <HexTileId as Tile<HexTileId>>::distribution() {
                for (tile, set_count) in grid.set_count_map().iter() {
                    let expected_set = total_set as f64
                        * dist_map
                            .get(tile)
                            .unwrap_or_else(|| panic!("expected distribution for {tile:?}"));
                    info!("set {tile:?} {set_count} / {expected_set:.2}");
                }
            }
        }
        events.regenerate_map.send(MapGenerated(
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
    wfc_animate: Res<WfcAnimate>,
    mut step_event_reader: EventReader<WfcStep>,
) {
    for _ in step_event_reader.read() {
        commands.run_system(gen_map_system_id.0);
    }
}

pub fn on_cell_selected_handler(
    mut commands: Commands,
    mut hex_possibilities_data: ResMut<SelectedGridCellData<HexTileId>>,
    hex_query: Query<(
        &GridCellPos,
        &GridCellTile<HexTileId>,
        &WfcCellPossibilities<HexTileId>,
        &Children,
    )>,
    pick_query: Query<(&Parent, Ref<PickSelection>), (Changed<PickSelection>, With<GridCellChild>)>,
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
            hex_possibilities_data.0.remove(pos);
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
                info!("updated cache for {pos:?}");
                hex_possibilities_data
                    .0
                    .entry(*pos)
                    .or_insert_with(|| (data.clone(), possibilities.clone()));
            }
        }
    }
}

pub fn invalid_hex_handler(
    hex_query: Query<(Ref<WfcGridCellInvalid>, &Children), Added<WfcGridCellInvalid>>,
    mut hex_segment_query: Query<
        (&mut Handle<ColorMaterial>, &InitialHighlight<ColorMaterial>),
        With<GridCellChild>,
    >,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut removed_invalids: RemovedComponents<WfcGridCellInvalid>,
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

pub fn regen_map_event_handler(
    mut commands: Commands,
    seed: Res<WfcSeed>,
    gen_map_system_id: Res<GenMapSystemId>,
    mut events: EventReader<RegenerateMap>,
    mut custom_rng: ResMut<CustomRng>,
) {
    if events.read().next().is_some() {
        events.clear();
        info!("seed: {:?}", seed.0);
        if let Some(seed) = seed.0 {
            if let Some(custom_rng) = custom_rng.0.as_mut() {
                *custom_rng =
                    StdRng::from_rng(StepRng::new(seed, 1)).expect("failed to create custom rng");
            }
        }
        commands.run_system(gen_map_system_id.0);
    }
}

pub fn on_clear_selection_handler(
    mut events: EventReader<ClearSelectedHex>,
    mut hex_possibilities_cache: ResMut<GridCellDataCache<HexTileId>>,
) {
    if events.read().next().is_some() {
        events.clear();
        hex_possibilities_cache.0.clear();
    }
}

pub fn grid_cell_set_event_handler(
    mut commands: Commands,
    mut grid_cell_set: EventReader<GridCellSet<HexTileId>>,
    hex_query: Query<(&GridCellPos, Entity)>,
    mut resources: GridEventHandlerResources,
) {
    let pos_entity_map = hex_query.iter().collect::<HashMap<_, _>>();
    for gcs in grid_cell_set.read() {
        if let Some(&entity) = pos_entity_map.get(&GridCellPos(gcs.pos)) {
            commands.entity(entity).despawn_recursive();
        }
        create_hex(
            CreateHexOptions {
                // TODO: this should be an object not tuple
                // we populate these when the grid is done
                possibilities: resources
                    .hex_possibilities_cache
                    .0
                    .get(&GridCellPos(gcs.pos))
                    .expect("failed to get possibilities")
                    .1
                     .0
                    .clone(),
                tile: gcs.tile,
                ix: gcs.pos.y as usize * resources.grid_size.0.x as usize + gcs.pos.x as usize,
                pos: gcs.pos,
                is_invalid: false,
            },
            &mut commands,
            &mut resources,
        );
    }
}

pub fn on_debug_text_added(
    mut commands: Commands,
    hex_text: Query<
        (Entity, &GridCellDebugText),
        Or<(Added<GridCellDebugText>, Changed<GridCellDebugText>)>,
    >,
    hex_text_mode: Res<GridCellDebugTextMode>,
) {
    let visibility = if hex_text_mode.0.is_some() {
        Visibility::Inherited
    } else {
        Visibility::Hidden
    };
    for (entity, GridCellDebugText(text)) in hex_text.iter() {
        commands.entity(entity).insert(Text2dBundle {
            text: Text::from_section(
                text,
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
    }
}

#[derive(SystemParam)]
struct GridEventHandlerResources<'w> {
    grid_size: Res<'w, GridSize>,
    hex_scale: Res<'w, GridCellScale>,
    hex_text_mode: Res<'w, GridCellDebugTextMode>,
    hex_possibilities_cache: Res<'w, GridCellDataCache<HexTileId>>,
    meshes: ResMut<'w, Assets<Mesh>>,
    materials: ResMut<'w, Assets<ColorMaterial>>,
    color_material_map: ResMut<'w, ColorMaterialMap>,
    hex_mesh: ResMut<'w, GridCellMeshHandle>,
}

pub fn grid_cell_refresh(
    mut commands: Commands,
    mut event_reader: EventReader<GridCellRefresh>,
    hex_query: Query<(&GridCellPos, (Entity, &Children))>,
    hex_text_query: Query<&GridCellDebugText>,
    mut resources: GridEventHandlerResources,
) {
    let pos_entity_map = hex_query.iter().collect::<HashMap<_, _>>();
    'outer: for &GridCellRefresh(pos) in event_reader.read() {
        let Some((data, possibilities)) =
            resources.hex_possibilities_cache.0.get(&GridCellPos(pos))
        else {
            panic!("bad event");
        };
        if let Some(&(entity, children)) = pos_entity_map.get(&GridCellPos(pos)) {
            for &child_entity in children {
                if hex_text_query.get(child_entity).is_err() {
                    continue;
                }
                if let Some((data, ..)) = resources.hex_possibilities_cache.0.get(&GridCellPos(pos))
                {
                    if data.0.is_none() {
                        // TODO: this is not ideal
                        if matches!(
                            resources.hex_text_mode.0,
                            Some(DebugTextMode::PossibilityCount)
                        ) {
                            commands
                                .entity(child_entity)
                                .insert(GridCellDebugText(possibilities.0.len().to_string()));
                        }
                    }
                }
            }
            continue 'outer;
        }
        create_hex(
            CreateHexOptions {
                // TODO: this should be an object not tuple
                // we populate these when the grid is done
                possibilities: resources
                    .hex_possibilities_cache
                    .0
                    .get(&GridCellPos(pos))
                    .expect("failed to get possibilities")
                    .1
                     .0
                    .clone(),
                tile: data.0,
                ix: pos.y as usize * resources.grid_size.0.x as usize + pos.x as usize,
                pos,
                is_invalid: false,
            },
            &mut commands,
            &mut resources,
        );
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
    commands: &mut Commands,
    resources: &mut GridEventHandlerResources,
) {
    let possibilities = create_hex_options.possibilities;
    let scale = resources.hex_scale.0;
    let tile = &create_hex_options.tile.clone();
    let pos = create_hex_options.pos;
    let ix = create_hex_options.ix;
    let x = pos.x;
    let y = pos.y;
    let translate_x = x as f32 * 1.5;
    let translate_y = y as f32 * 1.732;
    let mut position = Vec3::new(translate_x, translate_y, 0.0) * scale
        - Vec2::new(16.0, 20.0).extend(0.0) * scale;

    if x % 2 == 0 {
        position.y += 0.9 * scale;
    }
    let hex_sides: Option<FlatTopHexagonalSegmentIdMap> = tile.map(|tile| tile.into());
    let meshes = if let Some(mesh_handles) = resources.hex_mesh.0.as_ref() {
        mesh_handles.clone()
    } else {
        let meshes = crate::hex::mesh::hex_mesh(hex_sides).map(|mesh| {
            // AABBs don't get recalculated and break ray-casts / picking, so we need to either:
            // - resize mesh via mesh.scaled_by instead of transform.scale
            // - use transform.scale and call mesh.compute_aabb() after the mesh is loaded in the scene to fix it
            // https://github.com/bevyengine/bevy/issues/4294
            let mesh = mesh.scaled_by(Vec2::splat(0.95 * scale).extend(0.0));
            resources.meshes.add(mesh)
        });
        resources.hex_mesh.0 = Some(meshes.clone());
        meshes
    };
    let hex_text = match resources.hex_text_mode.0 {
        None => String::new(),
        Some(DebugTextMode::Index) => ix.to_string(),
        Some(DebugTextMode::PossibilityCount) => possibilities.len().to_string(),
    };
    let id = commands
        .spawn((
            GridCellTile(tile.clone()),
            GridCellIndex(ix),
            GridCellPos(UVec2::new(x, y)),
            WfcCellPossibilities(possibilities),
            SpatialBundle::from_transform(Transform::from_translation(position)),
        ))
        .with_children(|children| {
            for (ix, mesh_handle) in meshes.into_iter().enumerate() {
                let mesh = Mesh2dHandle(mesh_handle);
                let color = if let Some(sides) = hex_sides {
                    sides
                        .get_side_from_index(ix)
                        .expect("expected side")
                        .as_color()
                } else {
                    Color::BLACK
                };
                let handle = &resources
                    .color_material_map
                    .0
                    .entry(ColorWrapper(color))
                    .or_insert_with(|| resources.materials.add(ColorMaterial::from(color)));
                let material = (**handle).clone();
                children.spawn((
                    GridCellChild,
                    MaterialMesh2dBundle {
                        mesh,
                        material,
                        ..default()
                    },
                    PickableBundle::default(),
                ));
            }
            children.spawn(GridCellDebugText(hex_text));
        })
        .id();
    if create_hex_options.is_invalid {
        commands.entity(id).insert(WfcGridCellInvalid);
    }
}

pub fn on_map_generated_event_handler(
    mut commands: Commands,
    mut events: EventReader<MapGenerated<FlatTopHexGrid, HexTileId>>,
    hex_query: Query<(Entity, &GridCellPos), With<GridCellTile<HexTileId>>>,
    mut resources: GridEventHandlerResources,
) {
    if let Some(MapGenerated(wfc)) = events.read().last() {
        // despawn last map
        for (entity, _) in hex_query.iter() {
            commands.entity(entity).despawn_recursive();
        }

        let is_grid_valid = wfc.is_valid(false);
        info!("is valid: {is_grid_valid}");

        let empty_cell_count = wfc.grid().cells().iter().filter(|c| c.is_none()).count();
        info!("empty cells: {empty_cell_count}");

        let invalids: HashSet<(usize, usize)> = if !is_grid_valid {
            wfc.get_invalids(false).into_iter().collect()
        } else {
            HashSet::new()
        };

        for (ix, (cell, (possibilities))) in wfc
            .grid()
            .cells()
            .iter()
            .zip(wfc.cached_possibilities())
            .enumerate()
        {
            let (x, y) = wfc.grid().index_to_xy(ix);
            let is_hex_invalid = invalids.contains(&(x, y));
            create_hex(
                CreateHexOptions {
                    possibilities: possibilities.iter().cloned().collect(),
                    tile: *cell,
                    ix,
                    pos: UVec2::new(x as u32, y as u32),
                    is_invalid: is_hex_invalid,
                },
                &mut commands,
                &mut resources,
            );
        }
    }
}

#[derive(Debug, Copy, Clone, Event)]
pub struct ClearSelectedHex;

#[derive(Debug, Resource)]
pub struct GenMapSystemId(pub SystemId);

#[derive(Debug, Reflect, Resource)]
pub struct GridCellDataCache<T>(
    pub HashMap<GridCellPos, (GridCellTile<T>, WfcCellPossibilities<T>)>,
);

impl<T> Default for GridCellDataCache<T> {
    fn default() -> Self {
        Self(HashMap::default())
    }
}

#[derive(Debug, Reflect, Resource)]
pub struct SelectedGridCellData<T>(
    pub HashMap<GridCellPos, (GridCellTile<T>, WfcCellPossibilities<T>)>,
);

impl<T> Default for SelectedGridCellData<T> {
    fn default() -> Self {
        Self(HashMap::default())
    }
}

#[derive(Debug, Default, Resource)]
pub struct GridCellMeshHandle(pub Option<[Handle<Mesh>; 6]>);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Reflect, Component)]
pub struct GridCellPos(pub UVec2);

#[derive(Debug, Clone, Reflect, Component)]
pub struct GridCellTile<T>(pub Option<T>);

#[derive(Debug, Copy, Clone, Reflect, Component)]
pub struct GridCellIndex(pub usize);

#[derive(Debug, Clone, Reflect, Component)]
pub struct WfcCellPossibilities<T>(pub HashSet<T>);

#[derive(Debug, Copy, Clone, Component, Reflect)]
pub struct GridCellChild;

#[derive(Debug, Copy, Clone, Default, Reflect, Component)]
pub struct WfcGridCellInvalid;

#[derive(Debug, Default, Reflect, Component)]
pub struct GridCellDebugText(pub String);

#[derive(Debug, Copy, Clone, Event)]
pub struct RegenerateMap;

#[derive(Event)]
pub struct MapGenerated<GT, T>(pub WaveFunctionCollapse<GT, T>)
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>;

#[derive(Debug, Event)]
pub struct GridCellSet<T> {
    pub tile: Option<T>,
    pub pos: UVec2,
}

#[derive(Clone, Event)]
pub struct WfcStep;

#[derive(Clone, Event)]
pub struct GridCellRefresh(pub UVec2);
