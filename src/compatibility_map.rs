use std::collections::{BTreeMap, HashSet};
use std::fmt::Debug;

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{HexagonType, Polygon, Side, Tile};
use crate::grid::GridType;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Error)]
pub enum CompatibilityMapError<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    #[error("Contradiction: {0:?} allows for {2:?} on its {1:?} but {2:?} does not allow for {0:?} on its {3:?}"
    )]
    Contradiction(T, GT::SideType, T, GT::SideType),
}

pub type CompatibilityMapKey<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
= (GT::Type, T, GT::SideType);

#[derive(Clone, Serialize, Deserialize)]
pub struct CompatibilityMap<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    polygon: GT::Type,
    compatibility: BTreeMap<CompatibilityMapKey<GT, T>, HashSet<T>>,
}

impl<GT, T> Default for CompatibilityMap<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    fn default() -> Self {
        Self {
            polygon: GT::Type::default(),
            compatibility: BTreeMap::new(),
        }
    }
}

impl<GT, T> CompatibilityMap<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    pub fn new() -> Self {
        Self::default()
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
    pub fn estimated_size(&self) -> usize {
        std::mem::size_of_val(&self.polygon)
            + self
                .compatibility
                .iter()
                .map(|((k1, k2, k3), v)| {
                    std::mem::size_of_val(k1)
                        + std::mem::size_of_val(k2)
                        + std::mem::size_of_val(k3)
                        + v.iter().map(|v| std::mem::size_of_val(v)).sum::<usize>()
                })
                .sum::<usize>()
    }
    pub fn add(&mut self, tile: T, side: GT::SideType, compatible_tiles: Vec<T>) {
        self.compatibility
            .insert(self.key(tile, side), compatible_tiles.into_iter().collect());
    }
    pub fn get(&self, tile: T, side: GT::SideType) -> Option<&HashSet<T>> {
        self.compatibility.get(&self.key(tile, side))
    }
    pub fn check_contradictions(&self) -> Result<(), CompatibilityMapError<GT, T>> {
        for (&(polygon, id, side), valid_set) in self.compatibility.iter() {
            let opposite_side: Side = side.into();
            let opposite_side = opposite_side.opposite();
            let opposite_side: <GT as GridType<T>>::SideType = match opposite_side.try_into() {
                Ok(opposite_side) => opposite_side,
                // in theory this should unreachable with the compile time checks
                Err(_err) => panic!("failed to convert {opposite_side:?} back into SideType"),
            };
            for &other_id in valid_set {
                let err = CompatibilityMapError::Contradiction(id, side, other_id, opposite_side);
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

#[cfg(test)]
mod tests {
    use serde::{Deserialize, Serialize};

    use crate::{SquareSide, Tile, TileInstance};
    use crate::grid::SquareGrid;

    use super::*;

    #[test]
    fn test_check_contradiction() {
        #[derive(
            Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize,
        )]
        enum Id {
            A,
            B,
        }
        impl TileInstance for Id {}
        impl Tile<Self> for Id {
            fn all() -> Vec<Self> {
                vec![Self::A, Self::B]
            }
        }
        let mut map = CompatibilityMap::<SquareGrid, Id>::new();

        map.add(Id::A, SquareSide::Right, vec![Id::B]);
        map.add(Id::B, SquareSide::Left, vec![]);

        assert_eq!(
            map.check_contradictions(),
            Err(CompatibilityMapError::Contradiction(
                Id::A,
                SquareSide::Right,
                Id::B,
                SquareSide::Left,
            ))
        );

        map.add(Id::B, SquareSide::Left, vec![Id::A]);

        assert_eq!(map.check_contradictions(), Ok(()));
    }
}
