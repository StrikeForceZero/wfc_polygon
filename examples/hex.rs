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
struct HexGrid(Option<Grid<HexTile>>);

#[derive(Debug, Reflect, Component)]
struct Hex;

#[derive(Debug, Reflect, Component)]
struct HexPos(Vec2);

#[derive(Debug, Reflect, Component)]
struct HexData(FlatHexSides);

fn hex_mesh(sides: FlatHexSides) -> Vec<(Mesh, Color)> {
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
        HexTile::get_compatibility_map().expect("expected compat map"),
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
            let mut position = Vec3::new(x, y, 0.0);

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
                        Hex,
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
        app.register_type::<Hex>();
        app.register_type::<HexPos>();
        app.register_type::<HexData>();
        app.register_type::<HexTile>();
        app.register_type::<HexType>();
        app.register_type::<FlatHexSides>();
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

fn test() -> anyhow::Result<()> {
    let map = HexTile::get_compatibility_map()?;

    /*for (key, tiles) in map.compatibility.iter() {
        println!("{key:?}:");
        for tile in tiles {
            println!("\t{tile:?}");
        }
    }*/

    let mut grid = Grid::new(2, 2, map);
    let max_retries = 10;

    for n in 1..=max_retries {
        println!("attempt {n}/{max_retries}");
        if grid.collapse(Polygon::Hexagon(HexagonType::FlatTop))? {
            break;
        }
    }

    // grid.print();

    for row in grid.matrix() {
        for col in row {
            let values = if let Some(hex_col) = col {
                let sides: FlatHexSides = hex_col.into();
                [
                    [None, Some(sides.top), None],
                    [Some(sides.top_left), None, Some(sides.top_right)],
                    [Some(sides.bottom_left), None, Some(sides.bottom_right)],
                    [None, Some(sides.bottom), None],
                ]
            } else {
                [
                    [None, None, None],
                    [None, None, None],
                    [None, None, None],
                    [None, None, None],
                ]
            };
            for row in values {
                for col in row {
                    let v = if let Some(v) = col {
                        match v {
                            HexType::Grass => "G",
                            HexType::Water => "W",
                        }
                    } else {
                        "."
                    };
                    print!("{v}");
                }
                println!();
            }
        }
    }
    /*
     W    G
    W G  G W
    G G  G G
     G    W
     W    W
    W G  G W
    G W  G G
     W    W



     */
    Ok(())
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
enum HexType {
    Grass,
    Water,
}

impl HexType {
    pub fn as_color(&self) -> Color {
        match self {
            HexType::Grass => Color::BLUE,
            HexType::Water => Color::DARK_GREEN,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
struct FlatHexSides {
    top: HexType,
    top_left: HexType,
    top_right: HexType,
    bottom_left: HexType,
    bottom_right: HexType,
    bottom: HexType,
}

impl FlatHexSides {
    pub fn get_side_from_index(&self, index: usize) -> Option<HexType> {
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
}

type HexTypeTuple = (HexType, HexType, HexType, HexType, HexType, HexType);
const HEX_TYPE_TUPLE_GRASS: HexTypeTuple = (
    HexType::Grass,
    HexType::Grass,
    HexType::Grass,
    HexType::Grass,
    HexType::Grass,
    HexType::Grass,
);
const HEX_TYPE_TUPLE_WATER: HexTypeTuple = (
    HexType::Water,
    HexType::Water,
    HexType::Water,
    HexType::Water,
    HexType::Water,
    HexType::Water,
);
type HexTypeTupleOption = (
    Option<HexType>,
    Option<HexType>,
    Option<HexType>,
    Option<HexType>,
    Option<HexType>,
    Option<HexType>,
);

impl From<HexTypeTuple> for FlatHexSides {
    fn from(value: HexTypeTuple) -> Self {
        Self {
            top: value.0,
            top_left: value.1,
            top_right: value.2,
            bottom_left: value.3,
            bottom_right: value.4,
            bottom: value.5,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Default)]
struct FlatHexSidesPartial {
    top: Option<HexType>,
    top_left: Option<HexType>,
    top_right: Option<HexType>,
    bottom_left: Option<HexType>,
    bottom_right: Option<HexType>,
    bottom: Option<HexType>,
}

impl FlatHexSidesPartial {
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

impl From<HexTypeTupleOption> for FlatHexSidesPartial {
    fn from(value: HexTypeTupleOption) -> Self {
        Self {
            top: value.0,
            top_left: value.1,
            top_right: value.2,
            bottom_left: value.3,
            bottom_right: value.4,
            bottom: value.5,
        }
    }
}

impl From<HexTypeTuple> for FlatHexSidesPartial {
    fn from(value: HexTypeTuple) -> Self {
        Self {
            top: Some(value.0),
            top_left: Some(value.1),
            top_right: Some(value.2),
            bottom_left: Some(value.3),
            bottom_right: Some(value.4),
            bottom: Some(value.5),
        }
    }
}

impl From<FlatHexSides> for FlatHexSidesPartial {
    fn from(value: FlatHexSides) -> Self {
        Self {
            top: Some(value.top),
            top_left: Some(value.top_left),
            top_right: Some(value.top_right),
            bottom_left: Some(value.bottom_left),
            bottom_right: Some(value.bottom_right),
            bottom: Some(value.bottom),
        }
    }
}

impl From<FlatHexSides> for HexTypeTuple {
    fn from(value: FlatHexSides) -> Self {
        (
            value.top,
            value.top_left,
            value.top_right,
            value.bottom_left,
            value.bottom_right,
            value.bottom,
        )
    }
}

impl From<FlatHexSidesPartial> for HexTypeTupleOption {
    fn from(value: FlatHexSidesPartial) -> Self {
        (
            value.top,
            value.top_left,
            value.top_right,
            value.bottom_left,
            value.bottom_right,
            value.bottom,
        )
    }
}

impl From<HexTile> for HexTypeTuple {
    fn from(value: HexTile) -> Self {
        match value {
            HexTile::GrassWater(sides) => sides,
        }
    }
}

impl From<HexTile> for FlatHexSides {
    fn from(value: HexTile) -> Self {
        match value {
            HexTile::GrassWater(sides) => sides.into(),
            // Hex::Grass => (
            //     HexType::Grass,
            //     HexType::Grass,
            //     HexType::Grass,
            //     HexType::Grass,
            //     HexType::Grass,
            //     HexType::Grass,
            // )
            //     .into(),
            // Hex::Water => (
            //     HexType::Water,
            //     HexType::Water,
            //     HexType::Water,
            //     HexType::Water,
            //     HexType::Water,
            //     HexType::Water,
            // )
            //     .into(),
        }
    }
}

impl From<HexTile> for FlatHexSidesPartial {
    fn from(value: HexTile) -> Self {
        FlatHexSides::from(value).into()
    }
}

impl From<FlatHexSides> for HexTile {
    fn from(value: FlatHexSides) -> Self {
        /*match <PointyHexSides as From<HexTypeTuple>>::from(value) {
            // HEX_TYPE_TUPLE_GRASS => Hex::Grass,
            // HEX_TYPE_TUPLE_WATER => Hex::Water,
            sides => Hex::GrassWater(HexTypeTuple::from(sides)),
        }*/
        HexTile::GrassWater((
            value.top,
            value.top_left,
            value.top_right,
            value.bottom_left,
            value.bottom_right,
            value.bottom,
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
enum HexTile {
    GrassWater(HexTypeTuple),
    // Grass,
    // Water,
}

impl HexTile {
    fn grass_water_permutations() -> Vec<HexTile> {
        // Create a list of all possible HexTypes
        let hex_types = vec![HexType::Grass, HexType::Water];

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
            .map(|(a, b, c, d, e, f)| HexTile::GrassWater((a, b, c, d, e, f)))
            .collect()
    }
    fn valid_sets_pattern(sides: impl Into<FlatHexSides>) -> Vec<(Side, FlatHexSidesPartial)> {
        let sides: FlatHexSides = sides.into();
        vec![
            (
                Side::Top,
                FlatHexSidesPartial {
                    top: Some(sides.bottom),
                    ..Default::default()
                },
            ),
            (
                Side::TopRight,
                FlatHexSidesPartial {
                    top_right: Some(sides.bottom_left),
                    ..Default::default()
                },
            ),
            (
                Side::TopLeft,
                FlatHexSidesPartial {
                    top_left: Some(sides.bottom_right),
                    ..Default::default()
                },
            ),
            (
                Side::Bottom,
                FlatHexSidesPartial {
                    bottom: Some(sides.top),
                    ..Default::default()
                },
            ),
            (
                Side::BottomRight,
                FlatHexSidesPartial {
                    bottom_right: Some(sides.top_left),
                    ..Default::default()
                },
            ),
            (
                Side::BottomLeft,
                FlatHexSidesPartial {
                    bottom_left: Some(sides.top_right),
                    ..Default::default()
                },
            ),
        ]
    }
    fn get_compatibility_map() -> anyhow::Result<CompatibilityMap<Self>> {
        let mut map = CompatibilityMap::new(Polygon::Hexagon(HexagonType::FlatTop));
        let permutations = Self::all();
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

impl Display for HexTile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            HexTile::GrassWater(_) => "O",
            // Hex::Grass => "G",
            // Hex::Water => "W",
        };
        write!(f, "{str}")
    }
}

impl TileInstance for HexTile {}

impl Tile<Self> for HexTile {
    fn all() -> Vec<Self> {
        Self::grass_water_permutations()
    }
}
