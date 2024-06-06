use bevy::prelude::*;

use wfc_polygon::FlatTopHexSide;

use crate::hex::{FLAT_HEX_SIDES, HexSegmentIdTuple};
use crate::hex::segment::HexSegmentId;
use crate::hex::tile_id::HexTileId;
use crate::util::IsSomeAndSame;

impl From<HexTileId> for FlatTopHexagonalSegmentIdMap {
    fn from(value: HexTileId) -> Self {
        value.0.into()
    }
}

impl From<HexSegmentIdTuple> for FlatTopHexagonalSegmentIdMap {
    fn from(value: HexSegmentIdTuple) -> Self {
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
pub struct FlatTopHexagonalSegmentIdMap {
    pub top: HexSegmentId,
    pub top_right: HexSegmentId,
    pub bottom_right: HexSegmentId,
    pub bottom: HexSegmentId,
    pub bottom_left: HexSegmentId,
    pub top_left: HexSegmentId,
}

impl FlatTopHexagonalSegmentIdMap {
    pub fn as_id(&self) -> HexTileId {
        HexTileId((
            self.top,
            self.top_right,
            self.bottom_right,
            self.bottom,
            self.bottom_left,
            self.top_left,
        ))
    }
    pub fn get_side_from_index(&self, index: usize) -> Option<HexSegmentId> {
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
    pub fn get_side(&self, side: FlatTopHexSide) -> HexSegmentId {
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
    pub fn has_valid_adjacent_segments(&self) -> bool {
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
    pub fn has_valid_segments(&self) -> bool {
        for side in FLAT_HEX_SIDES {
            let seg = self.get_side(side);
            match seg {
                HexSegmentId::None => {}
                HexSegmentId::Grass => {}
                HexSegmentId::Mountain => {}
                HexSegmentId::Ocean | HexSegmentId::MountainPeak => {
                    for other_side in FLAT_HEX_SIDES {
                        if self.get_side(other_side) != seg {
                            return false;
                        }
                    }
                }
                HexSegmentId::River => {
                    // if FLAT_HEX_SIDES
                    //     .into_iter()
                    //     .filter(|&side| self.get_side(side) == seg)
                    //     .count()
                    //     > 2
                    // {
                    //     return false;
                    // }
                }
                HexSegmentId::Sand => {}
            }
            let compatible = self.get_side(side).compatible();
            for other_side in FLAT_HEX_SIDES {
                if !compatible.contains(&self.get_side(other_side)) {
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
pub struct FlatTopHexagonalSegmentIdOptionMap {
    pub top: Option<HexSegmentId>,
    pub top_right: Option<HexSegmentId>,
    pub bottom_right: Option<HexSegmentId>,
    pub bottom: Option<HexSegmentId>,
    pub bottom_left: Option<HexSegmentId>,
    pub top_left: Option<HexSegmentId>,
}

impl FlatTopHexagonalSegmentIdOptionMap {
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
    pub fn is_valid_adjacent(&self, side: FlatTopHexSide, other: Self) -> bool {
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

impl From<HexTileId> for FlatTopHexagonalSegmentIdOptionMap {
    fn from(value: HexTileId) -> Self {
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
