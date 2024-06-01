use std::collections::{BTreeMap, HashSet};

use thiserror::Error;

use crate::{HexagonType, Polygon, Side, TileInstance};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Error)]
pub enum CompatibilityMapError {
    #[error("{1:?} is an invalid side for {0:?}")]
    InvalidSide(Polygon, Side),
}

pub type CompatibilityMapKey<T> = (Polygon, T, Side);

#[derive(Debug, Clone)]
pub struct CompatibilityMap<T> {
    polygon: Polygon,
    compatibility: BTreeMap<CompatibilityMapKey<T>, HashSet<T>>,
}

impl<T> CompatibilityMap<T>
where
    T: TileInstance,
{
    pub fn new(polygon: Polygon) -> Self {
        Self {
            polygon,
            compatibility: BTreeMap::new(),
        }
    }
    #[inline]
    fn key(&self, tile: T, side: Side) -> CompatibilityMapKey<T> {
        (self.polygon, tile, side)
    }
    #[inline]
    fn is_valid_side(&self, side: Side) -> bool {
        match self.polygon {
            Polygon::Triangle => match side {
                Side::Bottom | Side::TopLeft | Side::TopRight => true,
                _ => false,
            },
            Polygon::Square => match side {
                Side::Top | Side::Bottom | Side::Left | Side::Right => true,
                _ => false,
            },
            Polygon::Hexagon(HexagonType::FlatTop) => match side {
                Side::Top
                | Side::TopLeft
                | Side::TopRight
                | Side::BottomLeft
                | Side::BottomRight
                | Side::Bottom => true,
                _ => false,
            },
            Polygon::Hexagon(HexagonType::PointyTop) => match side {
                Side::TopLeft
                | Side::TopRight
                | Side::BottomLeft
                | Side::BottomRight
                | Side::Left
                | Side::Right => true,
                _ => false,
            },
        }
    }
    pub fn add(
        &mut self,
        tile: T,
        side: Side,
        compatible_tiles: Vec<T>,
    ) -> Result<(), CompatibilityMapError> {
        if self.is_valid_side(side) {
            self.compatibility
                .insert(self.key(tile, side), compatible_tiles.into_iter().collect());
            Ok(())
        } else {
            Err(CompatibilityMapError::InvalidSide(self.polygon, side))
        }
    }
    pub fn get(&self, tile: T, side: Side) -> Result<Option<&HashSet<T>>, CompatibilityMapError> {
        if self.is_valid_side(side) {
            Ok(self.compatibility.get(&self.key(tile, side)))
        } else {
            Err(CompatibilityMapError::InvalidSide(self.polygon, side))
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = (&CompatibilityMapKey<T>, &HashSet<T>)> {
        self.compatibility.iter()
    }
}
