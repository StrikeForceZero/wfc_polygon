use bevy::prelude::*;
use itertools::iproduct;

use wfc_polygon::{FlatTopHexSide, Tile, TileInstance};
use wfc_polygon::compatibility_map::CompatibilityMap;
use wfc_polygon::grid::FlatTopHexGrid;

use crate::{Mode, MODE};
use crate::hex::HexSegmentIdTuple;
use crate::hex::map::{FlatTopHexagonalSegmentIdMap, FlatTopHexagonalSegmentIdOptionMap};
use crate::hex::segment::HexSegmentId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
pub struct HexTileId(pub HexSegmentIdTuple);

impl TileInstance for HexTileId {}

impl Tile<Self> for HexTileId {
    fn all() -> Vec<Self> {
        Self::permutations()
    }
}

impl HexTileId {
    pub fn permutations() -> Vec<HexTileId> {
        // Create a list of all possible HexTypes
        let hex_types = HexSegmentId::all();

        // Generate all permutations for the six sides of the hexagon
        let permutations = iproduct!(
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned(),
            hex_types.iter().cloned()
        );

        let iter = permutations.map(|(a, b, c, d, e, f)| HexTileId((a, b, c, d, e, f)));
        match MODE {
            Mode::Full => iter
                .filter(|&hex| FlatTopHexagonalSegmentIdMap::from(hex).is_all_segments_same())
                .collect(),
            Mode::Segments => iter
                .filter(|&hex| FlatTopHexagonalSegmentIdMap::from(hex).has_valid_segments())
                .collect(),
        }
    }
    pub fn valid_sets_pattern(
        sides: impl Into<FlatTopHexagonalSegmentIdMap>,
    ) -> Vec<(FlatTopHexSide, FlatTopHexagonalSegmentIdOptionMap)> {
        let sides: FlatTopHexagonalSegmentIdMap = sides.into();
        vec![
            (
                FlatTopHexSide::Top,
                FlatTopHexagonalSegmentIdOptionMap {
                    top: Some(sides.bottom),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::TopRight,
                FlatTopHexagonalSegmentIdOptionMap {
                    top_right: Some(sides.bottom_left),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::TopLeft,
                FlatTopHexagonalSegmentIdOptionMap {
                    top_left: Some(sides.bottom_right),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::Bottom,
                FlatTopHexagonalSegmentIdOptionMap {
                    bottom: Some(sides.top),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::BottomRight,
                FlatTopHexagonalSegmentIdOptionMap {
                    bottom_right: Some(sides.top_left),
                    ..Default::default()
                },
            ),
            (
                FlatTopHexSide::BottomLeft,
                FlatTopHexagonalSegmentIdOptionMap {
                    bottom_left: Some(sides.top_right),
                    ..Default::default()
                },
            ),
        ]
    }
    pub fn get_compatibility_map() -> CompatibilityMap<FlatTopHexGrid, Self> {
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