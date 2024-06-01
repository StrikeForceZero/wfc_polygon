use std::collections::{BTreeMap, HashSet};

use thiserror::Error;

use crate::{HexagonType, Polygon, Side, Tile};
use crate::grid::GridType;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Error)]
pub enum CompatibilityMapError {
    #[error("{1:?} is an invalid side for {0:?}")]
    InvalidSide(Polygon, Side),
}

pub type CompatibilityMapKey<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
= (GT::Type, T, GT::SideType);

#[derive(Clone)]
pub struct CompatibilityMap<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    polygon: GT::Type,
    compatibility: BTreeMap<CompatibilityMapKey<GT, T>, HashSet<T>>,
}

impl<GT, T> CompatibilityMap<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    pub fn new() -> Self {
        Self {
            polygon: GT::Type::default(),
            compatibility: BTreeMap::new(),
        }
    }
    #[inline]
    fn key(&self, tile: T, side: GT::SideType) -> CompatibilityMapKey<GT, T> {
        (self.polygon, tile, side)
    }
    #[inline]
    fn is_valid_side(&self, side: GT::SideType) -> bool {
        let side = side.into();
        match self.polygon.into() {
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
        side: GT::SideType,
        compatible_tiles: Vec<T>,
    ) -> Result<(), CompatibilityMapError> {
        if self.is_valid_side(side) {
            self.compatibility
                .insert(self.key(tile, side), compatible_tiles.into_iter().collect());
            Ok(())
        } else {
            Err(CompatibilityMapError::InvalidSide(
                self.polygon.into(),
                side.into(),
            ))
        }
    }
    pub fn get(
        &self,
        tile: T,
        side: GT::SideType,
    ) -> Result<Option<&HashSet<T>>, CompatibilityMapError> {
        if self.is_valid_side(side) {
            Ok(self.compatibility.get(&self.key(tile, side)))
        } else {
            Err(CompatibilityMapError::InvalidSide(
                self.polygon.into(),
                side.into(),
            ))
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = (&CompatibilityMapKey<GT, T>, &HashSet<T>)> {
        self.compatibility.iter()
    }
}
