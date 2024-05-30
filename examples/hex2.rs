use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

use bevy::prelude::*;
use bevy::render::mesh::Indices;
use bevy::render::mesh::PrimitiveTopology::TriangleList;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::utils::HashMap;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use itertools::iproduct;

use wfc_polygon::{CompatibilityMap, Grid, HexagonType, Polygon, Side, Tile, TileInstance};

#[derive(Debug, Default, Resource)]
struct HexGrid(Option<Grid<Hex>>);

#[derive(Debug, Reflect, Component)]
struct HexPos(Vec2);

#[derive(Debug, Reflect, Component)]
struct HexData(FlatHexSegments);

fn hex_mesh(sides: FlatHexSegments) -> Vec<(Mesh, Color)> {
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

            (
                mesh,
                sides
                    .get_side_from_index(n as usize)
                    .expect("expected side")
                    .as_color(),
            )
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

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut hex_grid: ResMut<HexGrid>,
) {
    commands.spawn(Camera2dBundle {
        projection: OrthographicProjection {
            far: 1000.,
            near: -1000.,
            scale: 0.05,
            ..default()
        },
        ..default()
    });
    let mut grid = Grid::new(
        25,
        25,
        Hex::get_compatibility_map().expect("expected compat map"),
    );
    let max_retries = 10;
    for n in 1..=max_retries {
        println!("attempt {n}/{max_retries}");
        if grid
            .collapse(Polygon::Hexagon(HexagonType::FlatTop))
            .expect("collapse failed")
        {
            println!("collapse successful");
            break;
        }
    }
    let mut color_materials: HashMap<ColorWrapper, Handle<ColorMaterial>> = HashMap::new();
    for (row_ix, row) in grid.matrix().iter().enumerate() {
        for (col_ix, col) in row.into_iter().enumerate() {
            let x = col_ix as f32 * 1.5;
            let y = row_ix as f32 * 1.732;
            let tile = col.expect("expected tile");
            let mut position = Vec3::new(x, y, 0.0) - Vec2::new(16.0, 20.0).extend(0.0);

            if col_ix % 2 == 0 {
                position.y += 0.9;
            }
            let mesh_color_tuples = hex_mesh(tile.into());
            for (mesh, color) in mesh_color_tuples {
                let mesh = Mesh2dHandle(meshes.add(mesh));
                let handle = &*color_materials
                    .entry(ColorWrapper(color))
                    .or_insert_with(|| materials.add(ColorMaterial::from(color)));
                let material = handle.clone();
                commands
                    .spawn((
                        HexData(tile.into()),
                        HexPos(Vec2::new(x, y)),
                        SpatialBundle {
                            transform: Transform::from_translation(position),
                            ..default()
                        },
                    ))
                    .with_children(|children| {
                        children.spawn(MaterialMesh2dBundle {
                            mesh,
                            material,
                            transform: Transform::from_scale(Vec2::splat(0.95).extend(0.0)),
                            ..default()
                        });
                    });
            }
        }
    }
    hex_grid.0 = Some(grid);
}

fn update() {}

struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<HexPos>();
        app.register_type::<HexData>();
        app.init_resource::<HexGrid>();
        app.add_systems(Startup, setup);
        // app.add_systems(Update, update);
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(WorldInspectorPlugin::default())
        .add_plugins(SubPlugin)
        .run();
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
            Self::MountainPeak => vec![Self::MountainPeak, Self::Mountain, Self::Creek],
            Self::River => vec![Self::Creek, Self::River, Self::Ocean],
            Self::Creek => vec![Self::Creek, Self::River, Self::Ocean, Self::MountainPeak],
            Self::Ocean => vec![Self::Ocean, Self::River, Self::Sand],
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

const FLAT_HEX_SIDES: [Side; 6] = [
    Side::Top,
    Side::TopRight,
    Side::BottomRight,
    Side::Bottom,
    Side::BottomLeft,
    Side::TopLeft,
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

impl Display for Hex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

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

        permutations
            .map(|(a, b, c, d, e, f)| Hex((a, b, c, d, e, f)))
            .filter(|&hex| FlatHexSegments::from(hex).has_valid_segments())
            .collect()
    }
    fn valid_sets_pattern(sides: impl Into<FlatHexSegments>) -> Vec<(Side, FlatHexSegmentsOpt)> {
        let sides: FlatHexSegments = sides.into();
        vec![
            (
                Side::Top,
                FlatHexSegmentsOpt {
                    top: Some(sides.bottom),
                    ..Default::default()
                },
            ),
            (
                Side::TopRight,
                FlatHexSegmentsOpt {
                    top_right: Some(sides.bottom_left),
                    ..Default::default()
                },
            ),
            (
                Side::TopLeft,
                FlatHexSegmentsOpt {
                    top_left: Some(sides.bottom_right),
                    ..Default::default()
                },
            ),
            (
                Side::Bottom,
                FlatHexSegmentsOpt {
                    bottom: Some(sides.top),
                    ..Default::default()
                },
            ),
            (
                Side::BottomRight,
                FlatHexSegmentsOpt {
                    bottom_right: Some(sides.top_left),
                    ..Default::default()
                },
            ),
            (
                Side::BottomLeft,
                FlatHexSegmentsOpt {
                    bottom_left: Some(sides.top_right),
                    ..Default::default()
                },
            ),
        ]
    }
    fn get_compatibility_map() -> anyhow::Result<CompatibilityMap<Self>> {
        let mut map = CompatibilityMap::new(Polygon::Hexagon(HexagonType::FlatTop));
        let permutations = Self::permutations();
        for &combination in permutations.iter() {
            for (side, pattern) in Self::valid_sets_pattern(combination) {
                let valid_tiles = permutations
                    .iter()
                    .filter_map(|&p| {
                        if pattern.is_match(side, p.into()) {
                            Some(p)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                if valid_tiles.is_empty() {
                    panic!("failed to get valid tiles\nside: {side:?}\npattern: {pattern:?}");
                }
                map.add(combination, side, valid_tiles)?;
            }
        }
        Ok(map)
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
    pub fn get_side(&self, side: Side) -> HexSegment {
        match side {
            Side::Top => self.top,
            Side::TopRight => self.top_right,
            Side::BottomRight => self.bottom_right,
            Side::Bottom => self.bottom,
            Side::BottomLeft => self.bottom_left,
            Side::TopLeft => self.top_left,
            _ => panic!("invalid side {side:?}"),
        }
    }
    pub fn adjacent_segments(&self, side: Side) -> [Side; 2] {
        match side {
            Side::Top => [Side::TopLeft, Side::TopRight],
            Side::TopRight => [Side::Top, Side::BottomRight],
            Side::BottomRight => [Side::TopRight, Side::BottomLeft],
            Side::Bottom => [Side::BottomRight, Side::BottomLeft],
            Side::BottomLeft => [Side::BottomRight, Side::TopLeft],
            Side::TopLeft => [Side::BottomLeft, Side::Top],
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
    fn is_match(&self, side: Side, other: Self) -> bool {
        match side {
            Side::Top => self.top.is_some_and_same(&other.top),
            Side::TopLeft => self.top_left.is_some_and_same(&other.top_left),
            Side::TopRight => self.top_right.is_some_and_same(&other.top_right),
            Side::BottomLeft => self.bottom_left.is_some_and_same(&other.bottom_left),
            Side::BottomRight => self.bottom_right.is_some_and_same(&other.bottom_right),
            Side::Bottom => self.bottom.is_some_and_same(&other.bottom),
            _ => panic!("bad side {side:?}"),
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
