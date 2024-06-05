use std::collections::HashMap;

use bevy::prelude::*;
use itertools::iproduct;
use serde::{Deserialize, Serialize};

use wfc_polygon::{FlatTopHexSide, Tile, TileInstance};
use wfc_polygon::compatibility_map::CompatibilityMap;
use wfc_polygon::grid::FlatTopHexGrid;

use crate::{HEX_MODE, HexMode};
use crate::hex::HexSegmentIdTuple;
use crate::hex::map::{FlatTopHexagonalSegmentIdMap, FlatTopHexagonalSegmentIdOptionMap};
use crate::hex::segment::HexSegmentId;

#[derive(
    Default,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd,
    Reflect,
    Serialize,
    Deserialize,
)]
pub struct HexTileId(pub HexSegmentIdTuple);

impl HexTileId {
    fn probability(&self) -> f64 {
        let tuple = self.0;
        [tuple.0, tuple.1, tuple.2, tuple.3, tuple.4, tuple.5]
            .map(|t| t.probability())
            .iter()
            .sum::<f64>()
            / 6.0
    }
    fn distribution(&self) -> f64 {
        let tuple = self.0;
        [tuple.0, tuple.1, tuple.2, tuple.3, tuple.4, tuple.5]
            .map(|t| t.distribution())
            .iter()
            .sum::<f64>()
            / 6.0
    }
}

impl TileInstance for HexTileId {}

impl Tile<Self> for HexTileId {
    fn all() -> Vec<Self> {
        Self::permutations()
    }
    fn probability() -> HashMap<Self, f64> {
        Self::all()
            .into_iter()
            .map(|t| (t, t.probability()))
            .collect()
    }
    fn distribution() -> Option<HashMap<Self, f64>> {
        Some(
            Self::all()
                .into_iter()
                .map(|t| (t, t.distribution()))
                .collect(),
        )
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

        let mode = *HEX_MODE.read().unwrap();
        match mode {
            HexMode::Full => iter
                .filter(|&hex| FlatTopHexagonalSegmentIdMap::from(hex).is_all_segments_same())
                .collect(),
            HexMode::Segments => iter
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
        println!("permutations: {}", permutations.len());
        for (index, &combination) in permutations.iter().enumerate() {
            println!(
                "{}/{} ({:.2}%)",
                index,
                permutations.len(),
                index as f32 / permutations.len() as f32 * 100.0
            );
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
