use std::collections::{BTreeMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use rand::seq::SliceRandom;
use rand::thread_rng;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Polygon {
    Triangle,
    Square,
    Hexagon(HexagonType),
}

impl Polygon {
    pub fn sides(&self) -> Vec<Side> {
        match self {
            Polygon::Triangle => Vec::from([Side::Bottom, Side::TopLeft, Side::TopRight]),
            Polygon::Square => Vec::from([Side::Top, Side::Bottom, Side::Left, Side::Right]),
            Polygon::Hexagon(HexagonType::FlatTop) => Vec::from([
                Side::Top,
                Side::TopLeft,
                Side::TopRight,
                Side::BottomLeft,
                Side::BottomRight,
                Side::Bottom,
            ]),
            Polygon::Hexagon(HexagonType::PointyTop) => Vec::from([
                Side::TopLeft,
                Side::TopRight,
                Side::BottomLeft,
                Side::BottomRight,
                Side::Left,
                Side::Right,
            ]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum HexagonType {
    FlatTop,
    PointyTop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Side {
    Top,
    Bottom,
    Left,
    Right,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

pub trait TileInstance:
    Debug + Display + Clone + Copy + PartialEq + PartialOrd + Ord + Hash
{
}

pub trait Tile<T: TileInstance>: TileInstance {
    fn all() -> Vec<T>;
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Error)]
pub enum CompatibilityMapError {
    #[error("{1:?} is an invalid side for {0:?}")]
    InvalidSide(Polygon, Side),
}

type CompatibilityMapKey<T> = (Polygon, T, Side);

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

#[derive(Debug, Clone, PartialEq, Error)]
pub enum GridError {
    #[error("GridError: {0:?}")]
    CompatibilityMapError(#[from] CompatibilityMapError),
}

#[derive(Debug, Clone)]
pub struct Grid<T> {
    width: usize,
    height: usize,
    cells: Vec<Option<T>>,
    compatibility: CompatibilityMap<T>,
}

impl<T> Grid<T>
where
    T: Tile<T>,
{
    pub fn new(width: usize, height: usize, compatibility: CompatibilityMap<T>) -> Self {
        Self {
            width,
            height,
            cells: vec![None; width * height],
            compatibility,
        }
    }

    pub fn get(&self, x: usize, y: usize) -> Option<T> {
        if x < self.width && y < self.height {
            self.cells[y * self.width + x]
        } else {
            None
        }
    }

    pub fn set(&mut self, x: usize, y: usize, tile: T) {
        if x < self.width && y < self.height {
            self.cells[y * self.width + x] = Some(tile);
        }
    }

    pub fn index_to_xy(&self, index: usize) -> (usize, usize) {
        let y = index / self.width;
        let x = index % self.width;
        (x, y)
    }

    pub fn get_by_index(&self, index: usize) -> Option<T> {
        let (x, y) = self.index_to_xy(index);
        self.get(x, y)
    }

    pub fn get_neighbor_index(
        &self,
        x: usize,
        y: usize,
        side: Side,
        polygon: Polygon,
    ) -> Option<usize> {
        let x = x as i32;
        let y = y as i32;
        let width = self.width as i32;
        let index = match polygon {
            Polygon::Triangle => match side {
                Side::Bottom => (y + 1) * width + x,
                Side::TopLeft => (y - 1) * width + (x - 1),
                Side::TopRight => (y - 1) * width + (x + 1),
                _ => panic!(" invalid side {side:?} for {polygon:?}"),
            },
            Polygon::Square => match side {
                Side::Top => (y - 1) * width + x,
                Side::Bottom => (y + 1) * width + x,
                Side::Left => y * width + (x - 1),
                Side::Right => y * width + (x + 1),
                _ => panic!(" invalid side {side:?} for {polygon:?}"),
            },
            Polygon::Hexagon(HexagonType::FlatTop) => match side {
                Side::Top => (y - 1) * width + x,
                Side::Bottom => (y + 1) * width + x,
                Side::TopLeft => {
                    if x % 2 == 0 {
                        y * width + (x - 1)
                    } else {
                        (y - 1) * width + (x - 1)
                    }
                }
                Side::TopRight => {
                    if x % 2 == 0 {
                        y * width + (x + 1)
                    } else {
                        (y - 1) * width + (x + 1)
                    }
                }
                Side::BottomLeft => {
                    if x % 2 == 0 {
                        (y + 1) * width + (x - 1)
                    } else {
                        y * width + (x - 1)
                    }
                }
                Side::BottomRight => {
                    if x % 2 == 0 {
                        (y + 1) * width + (x + 1)
                    } else {
                        y * width + (x + 1)
                    }
                }
                _ => panic!(" invalid side {side:?} for {polygon:?}"),
            },
            Polygon::Hexagon(HexagonType::PointyTop) => match side {
                Side::TopLeft => {
                    if y % 2 == 0 {
                        (y - 1) * width + (x - 1)
                    } else {
                        (y - 1) * width + x
                    }
                }
                Side::TopRight => {
                    if y % 2 == 0 {
                        (y - 1) * width + x
                    } else {
                        (y - 1) * width + (x + 1)
                    }
                }
                Side::BottomLeft => {
                    if y % 2 == 0 {
                        (y + 1) * width + (x - 1)
                    } else {
                        (y + 1) * width + x
                    }
                }
                Side::BottomRight => {
                    if y % 2 == 0 {
                        (y + 1) * width + x
                    } else {
                        (y + 1) * width + (x + 1)
                    }
                }
                Side::Left => y * width + (x - 1),
                Side::Right => y * width + (x + 1),
                _ => panic!(" invalid side {side:?} for {polygon:?}"),
            },
        };
        if index >= 0 {
            let index = index as usize;
            if index < self.cells.len() {
                Some(index)
            } else {
                // overflow
                None
            }
        } else {
            // negative
            None
        }
    }

    pub fn neighbor_indexes(
        &self,
        x: usize,
        y: usize,
        polygon: Polygon,
    ) -> Vec<(Side, Option<usize>)> {
        match polygon {
            Polygon::Triangle => [Side::TopLeft, Side::TopRight, Side::Bottom]
                .map(|side| (side, self.get_neighbor_index(x, y, side, polygon)))
                .to_vec(),
            Polygon::Square => [Side::Top, Side::Right, Side::Bottom, Side::Left]
                .map(|side| (side, self.get_neighbor_index(x, y, side, polygon)))
                .to_vec(),
            Polygon::Hexagon(HexagonType::FlatTop) => [
                Side::Top,
                Side::TopRight,
                Side::TopLeft,
                Side::BottomRight,
                Side::BottomLeft,
                Side::Bottom,
            ]
            .map(|side| (side, self.get_neighbor_index(x, y, side, polygon)))
            .to_vec(),
            Polygon::Hexagon(HexagonType::PointyTop) => [
                Side::TopLeft,
                Side::TopRight,
                Side::Left,
                Side::Right,
                Side::BottomLeft,
                Side::BottomRight,
            ]
            .map(|side| (side, self.get_neighbor_index(x, y, side, polygon)))
            .to_vec(),
        }
    }

    pub fn neighbors(&self, x: usize, y: usize, polygon: Polygon) -> Vec<(Side, Option<T>)> {
        self.neighbor_indexes(x, y, polygon)
            .into_iter()
            .map(|(side, index)| (side, index.and_then(|index| self.get_by_index(index))))
            .collect()
    }

    pub fn collapse(&mut self, polygon: Polygon) -> Result<bool, GridError> {
        let mut rng = thread_rng();
        let mut possibilities: Vec<HashSet<T>> =
            vec![T::all().into_iter().collect(); self.width * self.height];

        // TODO: validate existing cells

        // Re-load possibilities each iteration to account for external changes
        for (ix, cell) in self.cells.iter().enumerate() {
            let Some(tile) = cell else {
                continue;
            };
            possibilities[ix].clear();
            let (x, y) = self.index_to_xy(ix);
            for (side, nix) in self.neighbor_indexes(x, y, polygon) {
                let Some(nix) = nix else {
                    continue;
                };
                if let Some(compatible) = self.compatibility.get(*tile, side)? {
                    possibilities[nix].retain(|p| compatible.contains(p))
                } else {
                    unreachable!("bad compatibility map?")
                }
            }
        }

        while let Some((index, _)) = possibilities
            .iter()
            .enumerate()
            .filter(|(_, set)| set.len() > 1)
            .min_by_key(|(_, set)| set.len())
        {
            let x = index % self.width;
            let y = index / self.width;

            if let Some(&tile) = possibilities[index]
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .choose(&mut rng)
            {
                self.set(x, y, tile);
                possibilities[index] = HashSet::new();
                possibilities[index].insert(tile);

                for (side, neighbor_index_opt) in self.neighbor_indexes(x, y, polygon) {
                    if let Some(neighbor_index) = neighbor_index_opt {
                        if let Some(allowed_tiles) = self.compatibility.get(tile, side)? {
                            possibilities[neighbor_index].retain(|t| allowed_tiles.contains(t));
                        }
                    }
                }
            }
        }

        // Check if any cells are still None
        for y in 0..self.height {
            for x in 0..self.width {
                let index = y * self.width + x;
                if self.cells[index].is_none() {
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    pub fn matrix(&self) -> Vec<Vec<Option<T>>> {
        let mut rows = Vec::with_capacity(self.height);
        for y in 0..self.height {
            let mut row = Vec::with_capacity(self.width);
            for x in 0..self.width {
                row.push(self.get(x, y));
            }
            rows.push(row);
        }
        rows
    }
}

impl<T> Display for Grid<T>
where
    T: Tile<T>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                let tile = match self.get(x, y) {
                    Some(tile) => tile.to_string(),
                    None => "?".to_string(), // This should not appear
                };
                write!(f, "{}", tile)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Formatter;

    use super::*;

    #[test]
    fn test_square() -> anyhow::Result<()> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
        enum MyTile {
            GrassWater,
            Grass,
            Water,
        }
        impl MyTile {
            fn compatible(&self) -> Vec<Self> {
                match self {
                    MyTile::GrassWater => vec![MyTile::GrassWater, MyTile::Grass, MyTile::Water],
                    MyTile::Grass => vec![MyTile::GrassWater, MyTile::Grass],
                    MyTile::Water => vec![MyTile::GrassWater, MyTile::Water],
                }
            }
        }
        impl Display for MyTile {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let str = match self {
                    MyTile::GrassWater => "▒",
                    MyTile::Grass => "▓",
                    MyTile::Water => "░",
                };
                write!(f, "{str}")
            }
        }

        impl TileInstance for MyTile {}

        impl Tile<Self> for MyTile {
            fn all() -> Vec<Self> {
                vec![MyTile::GrassWater, MyTile::Grass, MyTile::Water]
            }
        }

        let mut compatibility = CompatibilityMap::new(Polygon::Square);

        for tile in MyTile::all() {
            for side in [Side::Top, Side::Right, Side::Bottom, Side::Left] {
                compatibility.add(tile, side, tile.compatible())?;
            }
        }

        let mut grid = Grid::new(10, 10, compatibility);
        let max_retries = 10;

        for _ in 1..=max_retries {
            if grid.collapse(Polygon::Square)? {
                break;
            }
        }

        let mut actual_tile_count = 0;
        for (row_ix, row) in grid.matrix().into_iter().enumerate() {
            for (col_ix, col) in row.into_iter().enumerate() {
                let current_tile = col.expect("expected tile");
                let mut neighbors_to_check = vec![];
                neighbors_to_check.push(grid.get(col_ix + 1, row_ix));
                neighbors_to_check.push(grid.get(col_ix, row_ix + 1));
                if row_ix > 0 {
                    neighbors_to_check.push(grid.get(col_ix, row_ix - 1));
                }
                if col_ix > 0 {
                    neighbors_to_check.push(grid.get(col_ix - 1, row_ix));
                }
                for neighbor_tile in neighbors_to_check.into_iter().filter_map(|t| t) {
                    let compatible = current_tile.compatible();
                    assert!(
                        compatible.contains(&neighbor_tile),
                        "{current_tile:?} compatible tiles does not contain {neighbor_tile:?} ({compatible:?})"
                    );
                }
                actual_tile_count += 1;
            }
        }

        assert_eq!(actual_tile_count, grid.width * grid.height);

        Ok(())
    }
}
