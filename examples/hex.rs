use std::hash::{Hash, Hasher};

use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use bevy::render::mesh::Indices;
use bevy::render::mesh::PrimitiveTopology::TriangleList;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::utils::{HashMap, HashSet};
use bevy_inspector_egui::bevy_egui::{EguiContexts, EguiPlugin};
use bevy_inspector_egui::egui;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_mod_picking::highlight::InitialHighlight;
use bevy_mod_picking::prelude::*;
use itertools::iproduct;

use wfc_polygon::{FlatTopHexSide, HexagonType, Polygon, Side, Tile, TileInstance};
use wfc_polygon::compatibility_map::CompatibilityMap;
use wfc_polygon::grid::{FlatTopHexGrid, Grid, GridType};
use wfc_polygon::wfc::WaveFunctionCollapse;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Mode {
    Full,
    Segments,
}

const MODE: Mode = Mode::Full;

#[derive(Default, Resource)]
struct HexGrid(Option<WaveFunctionCollapse<FlatTopHexGrid, Hex>>);

#[derive(Debug, Copy, Clone, Reflect, Component, PartialEq, Eq, Hash)]
struct HexPos(UVec2);

#[derive(Debug, Clone, Reflect, Component)]
struct HexData(Option<FlatHexSegments>);

#[derive(Debug, Clone, Reflect, Component)]
struct HexPossibilities(HashSet<Hex>);

fn hex_mesh(sides: Option<FlatHexSegments>) -> Vec<(Mesh, Color)> {
    // pointy top
    // let vertices = &[
    //     // Center vertex
    //     [0.0, 0.0, 0.0],
    //     // Vertices of the hexagon
    //     [0.0, 1.0, 0.0],     // top
    //     [0.866, 0.5, 0.0],   // top-right
    //     [0.866, -0.5, 0.0],  // bottom-right
    //     [0.0, -1.0, 0.0],    // bottom
    //     [-0.866, -0.5, 0.0], // bottom-left
    //     [-0.866, 0.5, 0.0],  // top-left
    // ];

    // flat top
    let vertices = &[
        // Center vertex
        [0.0, 0.0, 0.0],
        // Vertices of the hexagon
        [1.0, 0.0, 0.0],     // right
        [0.5, 0.866, 0.0],   // top-right
        [-0.5, 0.866, 0.0],  // top-left
        [-1.0, 0.0, 0.0],    // left
        [-0.5, -0.866, 0.0], // bottom-left
        [0.5, -0.866, 0.0],  // bottom-right
    ];

    // Define the indices for the 6 triangles
    let indices = Indices::U32(vec![
        /* rustfmt keep multi line */
        0, 1, 2, // right
        0, 2, 3, // top-right
        0, 3, 4, // top-left
        0, 4, 5, // left
        0, 5, 6, // bottom-left
        0, 6, 1, // bottom-right
    ]);

    // Create the mesh
    let mut base_mesh = Mesh::new(TriangleList, RenderAssetUsages::default());
    base_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices.to_vec());
    base_mesh.insert_indices(indices);

    // Triangle 0: Top side
    // Triangle 1: Top-right side
    // Triangle 2: Bottom-right side
    // Triangle 3: Bottom side
    // Triangle 4: Bottom-left side
    // Triangle 5: Top-left side

    (0..6)
        .map(|n| {
            let mut mesh = base_mesh.clone();
            mesh.insert_indices(Indices::U32(vec![0, (n + 1) % 6 + 1, (n + 2) % 6 + 1]));

            let color = if let Some(sides) = sides {
                sides
                    .get_side_from_index(n as usize)
                    .expect("expected side")
                    .as_color()
            } else {
                Color::BLACK
            };
            (mesh, color)
        })
        .collect()
}

#[derive(Debug, Copy, Clone)]
struct ColorWrapper(Color);

impl PartialEq for ColorWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for ColorWrapper {}

impl ColorWrapper {
    pub fn color(&self) -> Color {
        self.0
    }
    pub fn id(&self) -> [usize; 4] {
        let r = (self.0.r() * 100.0) as usize;
        let g = (self.0.g() * 100.0) as usize;
        let b = (self.0.b() * 100.0) as usize;
        let a = (self.0.a() * 100.0) as usize;
        [r, g, b, a]
    }
}

impl Hash for ColorWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

#[derive(Debug, Resource)]
struct GenMapSystemId(SystemId);

const SCALE: f32 = 20.0;

fn setup(gen_map_system_id: Res<GenMapSystemId>, mut commands: Commands) {
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

fn gen_map(
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
        .into_iter()
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
        let hex_sides: Option<FlatHexSegments> = if let Some(tile) = *cell {
            Some(tile.into())
        } else {
            None
        };
        let mesh_color_tuples = hex_mesh(hex_sides);
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

fn input_handler(
    mut commands: Commands,
    mouse_input: Res<ButtonInput<MouseButton>>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    gen_map_system_id: Res<GenMapSystemId>,
) {
    if keyboard_input.pressed(KeyCode::ControlLeft) && mouse_input.just_pressed(MouseButton::Left) {
        commands.run_system(gen_map_system_id.0);
        return;
    }
}

fn update(
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

fn invalid_hex_handler(
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
                let Some((mut asset, initial)) = hex_segment_query.get_mut(*child).ok() else {
                    continue;
                };
                *asset = color_material_handle.to_owned();
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
            *asset = initial.initial.to_owned();
        }
    }
}

#[derive(Debug, Copy, Clone, Component, Reflect)]
struct InnerHex;

#[derive(Debug, Default, Reflect, Resource)]
struct HexPossibilitiesCache(HashMap<HexPos, (HexData, HexPossibilities)>);

#[derive(Debug, Default, Reflect, Component)]
struct HexInvalid;

struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(gen_map));
        app
            /* rustfmt next line chain */
            .insert_resource(gen_map_res)
            .register_type::<HexPos>()
            .register_type::<HexData>()
            .register_type::<HexPossibilities>()
            .register_type::<InnerHex>()
            .register_type::<HexInvalid>()
            .register_type::<HexPossibilitiesCache>()
            .init_resource::<HexPossibilitiesCache>()
            .init_resource::<HexGrid>()
            .add_systems(Startup, setup)
            .add_systems(Update,
                 (
                    ui,
                    update,
                    input_handler,
                ).chain()
            )
            .add_systems(PostUpdate, invalid_hex_handler)
            /* rustfmt next line semi-colon */
        ;
    }
}

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

fn ui(mut egui_contexts: EguiContexts, hex_possibilities_cache: Res<HexPossibilitiesCache>) {
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

pub trait IsSomeAndSame {
    fn is_some_and_same(&self, other: &Self) -> bool;
}
impl<T> IsSomeAndSame for Option<T>
where
    T: PartialEq,
{
    fn is_some_and_same(&self, other: &Self) -> bool {
        match (self, other) {
            (None, _) | (_, None) => false,
            (Some(a), Some(b)) => a == b,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
enum HexSegment {
    Grass,
    Mountain,
    MountainPeak,
    River,
    Creek,
    Ocean,
    Sand,
}

impl HexSegment {
    fn all() -> Vec<HexSegment> {
        vec![
            Self::Grass,
            Self::Mountain,
            Self::MountainPeak,
            Self::River,
            Self::Creek,
            Self::Ocean,
            Self::Sand,
        ]
    }
    fn compatible(&self) -> Vec<HexSegment> {
        match self {
            Self::Grass => vec![Self::Grass, Self::Sand, Self::Mountain],
            Self::Mountain => vec![Self::Mountain, Self::MountainPeak, Self::Grass],
            Self::MountainPeak => vec![Self::Mountain, Self::Creek],
            Self::River => vec![Self::River, Self::Creek, Self::Ocean],
            Self::Creek => vec![Self::Creek, Self::River, Self::Ocean, Self::MountainPeak],
            Self::Ocean => vec![Self::Ocean, Self::Creek, Self::River, Self::Sand],
            Self::Sand => vec![Self::Sand, Self::Ocean, Self::Grass],
        }
    }
    fn as_color(&self) -> Color {
        match self {
            HexSegment::Grass => Color::DARK_GREEN,
            HexSegment::Mountain => Color::GRAY,
            HexSegment::MountainPeak => Color::WHITE,
            HexSegment::River => Color::BLUE,
            HexSegment::Creek => Color::CYAN,
            HexSegment::Ocean => Color::MIDNIGHT_BLUE,
            HexSegment::Sand => Color::rgb(0.82, 0.70, 0.55), // Tan
        }
    }
}

const FLAT_HEX_SIDES: [FlatTopHexSide; 6] = [
    FlatTopHexSide::Top,
    FlatTopHexSide::TopRight,
    FlatTopHexSide::BottomRight,
    FlatTopHexSide::Bottom,
    FlatTopHexSide::BottomLeft,
    FlatTopHexSide::TopLeft,
];

type HexSegmentTuple = (
    HexSegment,
    HexSegment,
    HexSegment,
    HexSegment,
    HexSegment,
    HexSegment,
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
struct Hex(HexSegmentTuple);

impl TileInstance for Hex {}

impl Tile<Self> for Hex {
    fn all() -> Vec<Self> {
        Self::permutations()
    }
}

impl Hex {
    fn permutations() -> Vec<Hex> {
        // Create a list of all possible HexTypes
        let hex_types = HexSegment::all();

        // Generate all permutations for the six sides of the hexagon
        let permutations = iproduct!(
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned()
        );

        let iter = permutations.map(|(a, b, c, d, e, f)| Hex((a, b, c, d, e, f)));
        match MODE {
            Mode::Full => iter
                .filter(|&hex| FlatHexSegments::from(hex).is_all_segments_same())
                .collect(),
            Mode::Segments => iter
                .filter(|&hex| FlatHexSegments::from(hex).has_valid_segments())
                .collect(),
        }
    }
    fn valid_sets_pattern(
        sides: impl Into<FlatHexSegments>,
    ) -> Vec<(FlatTopHexSide, FlatHexSegmentsOpt)> {
        let sides: FlatHexSegments = sides.into();
        vec![
            (
                FlatTopHexSide::Top,
                FlatHexSegmentsOpt {
                    top: Some(sides.bottom),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::TopRight,
                FlatHexSegmentsOpt {
                    top_right: Some(sides.bottom_left),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::TopLeft,
                FlatHexSegmentsOpt {
                    top_left: Some(sides.bottom_right),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::Bottom,
                FlatHexSegmentsOpt {
                    bottom: Some(sides.top),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::BottomRight,
                FlatHexSegmentsOpt {
                    bottom_right: Some(sides.top_left),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::BottomLeft,
                FlatHexSegmentsOpt {
                    bottom_left: Some(sides.top_right),
                    ..Default::default()
                },
            ),
        ]
    }
    fn get_compatibility_map() -> CompatibilityMap<FlatTopHexGrid, Self> {
        let mut map = CompatibilityMap::new();
        let permutations = Self::permutations();
        for &combination in permutations.iter() {
            for (side, pattern) in Self::valid_sets_pattern(combination) {
                let valid_tiles = permutations
                    .iter()
                    .filter_map(|&p| {
                        if pattern.is_valid_adjacent(side, p.into()) {
                            Some(p)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                if valid_tiles.is_empty() {
                    panic!("failed to get valid tiles\nside: {side:?}\npattern: {pattern:?}");
                }
                map.add(combination, side, valid_tiles);
            }
        }
        map
    }
}

impl From<Hex> for FlatHexSegments {
    fn from(value: Hex) -> Self {
        value.0.into()
    }
}

impl From<HexSegmentTuple> for FlatHexSegments {
    fn from(value: HexSegmentTuple) -> Self {
        Self {
            top: value.0,
            top_right: value.1,
            bottom_right: value.2,
            bottom: value.3,
            bottom_left: value.4,
            top_left: value.5,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
struct FlatHexSegments {
    top: HexSegment,
    top_right: HexSegment,
    bottom_right: HexSegment,
    bottom: HexSegment,
    bottom_left: HexSegment,
    top_left: HexSegment,
}

impl FlatHexSegments {
    pub fn get_side_from_index(&self, index: usize) -> Option<HexSegment> {
        Some(match index {
            0 => self.top,
            1 => self.top_right,
            2 => self.bottom_right,
            3 => self.bottom,
            4 => self.bottom_left,
            5 => self.top_left,
            _ => panic!("invalid index {index}, expected 0..=5"),
        })
    }
    pub fn get_side(&self, side: FlatTopHexSide) -> HexSegment {
        match side {
            FlatTopHexSide::Top => self.top,
            FlatTopHexSide::TopRight => self.top_right,
            FlatTopHexSide::BottomRight => self.bottom_right,
            FlatTopHexSide::Bottom => self.bottom,
            FlatTopHexSide::BottomLeft => self.bottom_left,
            FlatTopHexSide::TopLeft => self.top_left,
            _ => panic!("invalid side {side:?}"),
        }
    }
    pub fn adjacent_segments(&self, side: FlatTopHexSide) -> [FlatTopHexSide; 2] {
        match side {
            FlatTopHexSide::Top => [FlatTopHexSide::TopLeft, FlatTopHexSide::TopRight],
            FlatTopHexSide::TopRight => [FlatTopHexSide::Top, FlatTopHexSide::BottomRight],
            FlatTopHexSide::BottomRight => [FlatTopHexSide::TopRight, FlatTopHexSide::BottomLeft],
            FlatTopHexSide::Bottom => [FlatTopHexSide::BottomRight, FlatTopHexSide::BottomLeft],
            FlatTopHexSide::BottomLeft => [FlatTopHexSide::BottomRight, FlatTopHexSide::TopLeft],
            FlatTopHexSide::TopLeft => [FlatTopHexSide::BottomLeft, FlatTopHexSide::Top],
            _ => panic!("invalid side {side:?}"),
        }
    }
    pub fn has_valid_segments(&self) -> bool {
        for side in FLAT_HEX_SIDES {
            let compatible = self.get_side(side).compatible();
            for adjacent in self.adjacent_segments(side) {
                if !compatible.contains(&self.get_side(adjacent)) {
                    return false;
                }
            }
        }
        true
    }
    pub fn is_all_segments_same(&self) -> bool {
        let mut last_seg_opt = None;
        for seg in FLAT_HEX_SIDES.map(|s| self.get_side(s)) {
            if let Some(last_seg) = last_seg_opt {
                if seg != last_seg {
                    return false;
                }
            } else {
                last_seg_opt = Some(seg);
            }
        }
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect, Default)]
struct FlatHexSegmentsOpt {
    top: Option<HexSegment>,
    top_right: Option<HexSegment>,
    bottom_right: Option<HexSegment>,
    bottom: Option<HexSegment>,
    bottom_left: Option<HexSegment>,
    top_left: Option<HexSegment>,
}

impl FlatHexSegmentsOpt {
    fn is_match(&self, side: FlatTopHexSide, other: Self) -> bool {
        match side {
            FlatTopHexSide::Top => self.top.is_some_and_same(&other.top),
            FlatTopHexSide::TopLeft => self.top_left.is_some_and_same(&other.top_left),
            FlatTopHexSide::TopRight => self.top_right.is_some_and_same(&other.top_right),
            FlatTopHexSide::BottomLeft => self.bottom_left.is_some_and_same(&other.bottom_left),
            FlatTopHexSide::BottomRight => self.bottom_right.is_some_and_same(&other.bottom_right),
            FlatTopHexSide::Bottom => self.bottom.is_some_and_same(&other.bottom),
        }
    }
    fn is_valid_adjacent(&self, side: FlatTopHexSide, other: Self) -> bool {
        match side {
            FlatTopHexSide::Top => self.top.zip(other.top).map_or(false, |(seg, other_seg)| {
                seg.compatible().contains(&other_seg)
            }),
            FlatTopHexSide::TopLeft => self
                .top_left
                .zip(other.top_left)
                .map_or(false, |(seg, other_seg)| {
                    seg.compatible().contains(&other_seg)
                }),
            FlatTopHexSide::TopRight => self
                .top_right
                .zip(other.top_right)
                .map_or(false, |(seg, other_seg)| {
                    seg.compatible().contains(&other_seg)
                }),
            FlatTopHexSide::BottomLeft => self
                .bottom_left
                .zip(other.bottom_left)
                .map_or(false, |(seg, other_seg)| {
                    seg.compatible().contains(&other_seg)
                }),
            FlatTopHexSide::BottomRight => self
                .bottom_right
                .zip(other.bottom_right)
                .map_or(false, |(seg, other_seg)| {
                    seg.compatible().contains(&other_seg)
                }),
            FlatTopHexSide::Bottom => self
                .bottom
                .zip(other.bottom)
                .map_or(false, |(seg, other_seg)| {
                    seg.compatible().contains(&other_seg)
                }),
        }
    }
}

impl From<Hex> for FlatHexSegmentsOpt {
    fn from(value: Hex) -> Self {
        Self {
            top: Some(value.0 .0),
            top_right: Some(value.0 .1),
            bottom_right: Some(value.0 .2),
            bottom: Some(value.0 .3),
            bottom_left: Some(value.0 .4),
            top_left: Some(value.0 .5),
        }
    }
}
