use std::hash::{Hash, Hasher};

use bevy::prelude::*;
use bevy::render::mesh::Indices;
use bevy::render::mesh::PrimitiveTopology::TriangleList;
use bevy::render::render_asset::RenderAssetUsages;
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_mod_picking::prelude::*;
use itertools::iproduct;

use component::{HexData, HexGrid, HexInvalid, HexPos, HexPossibilities, InnerHex};
use resource::{GenMapSystemId, HexPossibilitiesCache};
use wfc_polygon::{FlatTopHexSide, Tile, TileInstance};
use wfc_polygon::compatibility_map::CompatibilityMap;
use wfc_polygon::grid::FlatTopHexGrid;

mod component;
mod resource;
mod system;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Mode {
    Full,
    Segments,
}

const MODE: Mode = Mode::Full;

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

const SCALE: f32 = 20.0;

struct SubPlugin;

impl Plugin for SubPlugin {
    fn build(&self, app: &mut App) {
        let gen_map_res = GenMapSystemId(app.world.register_system(system::gen_map));
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
            .add_systems(Startup, system::setup)
            .add_systems(Update,
                         (
                             system::ui,
                             system::update,
                             system::input_handler,
                         ).chain()
            )
            .add_systems(PostUpdate, system::invalid_hex_handler)
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
