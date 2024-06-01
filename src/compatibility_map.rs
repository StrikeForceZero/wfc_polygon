use std::collections::{BTreeMap, HashSet};

use thiserror::Error;

use crate::{HexagonType, Polygon, Side, Tile};
use crate::grid::GridType;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Error)]
pub enum CompatibilityMapError<T> {
    #[error("{1:?} is an invalid side for {0:?}")]
    InvalidSide(Polygon, Side),
    #[error("Contradiction: {0:?} allows for {2:?} on its {1:?} but {2:?} does not allow for {0:?} on its {3:?}"
    )]
    Contradiction(T, Side, T, Side),
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
            Polygon::Triangle => matches!(side, Side::Bottom | Side::TopLeft | Side::TopRight),
            Polygon::Square => matches!(side, Side::Top | Side::Bottom | Side::Left | Side::Right),
            Polygon::Hexagon(HexagonType::FlatTop) => matches!(
                side,
                Side::Top
                    | Side::TopLeft
                    | Side::TopRight
                    | Side::BottomLeft
                    | Side::BottomRight
                    | Side::Bottom
            ),
            Polygon::Hexagon(HexagonType::PointyTop) => matches!(
                side,
                Side::TopLeft
                    | Side::TopRight
                    | Side::BottomLeft
                    | Side::BottomRight
                    | Side::Left
                    | Side::Right
            ),
        }
    }
    pub fn add(
        &mut self,
        tile: T,
        side: GT::SideType,
        compatible_tiles: Vec<T>,
    ) -> Result<(), CompatibilityMapError<T>> {
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
    ) -> Result<Option<&HashSet<T>>, CompatibilityMapError<T>> {
        if self.is_valid_side(side) {
            Ok(self.compatibility.get(&self.key(tile, side)))
        } else {
            Err(CompatibilityMapError::InvalidSide(
                self.polygon.into(),
                side.into(),
            ))
        }
    }
    pub fn check_contradictions(&self) -> Result<(), CompatibilityMapError<T>> {
        for (&(polygon, id, side), valid_set) in self.compatibility.iter() {
            let opposite_side: Side = side.into();
            let opposite_side = opposite_side.opposite();
            let opposite_side: <GT as GridType<T>>::SideType = match opposite_side.try_into() {
                Ok(opposite_side) => opposite_side,
                // in theory this should unreachable with the compile time checks
                Err(err) => panic!("failed to convert {opposite_side:?} back into SideType"),
            };
            for &other_id in valid_set {
                let err = CompatibilityMapError::Contradiction(
                    id,
                    side.into(),
                    other_id,
                    opposite_side.into(),
                );
                let other_key = &(polygon, other_id, opposite_side);
                if let Some(other_valid_set) = self.compatibility.get(other_key) {
                    if other_valid_set.contains(&id) {
                        continue;
                    }
                }
                return Err(err);
            }
        }
        Ok(())
    }
    pub fn iter(&self) -> impl Iterator<Item = (&CompatibilityMapKey<GT, T>, &HashSet<T>)> {
        self.compatibility.iter()
    }
}
