use std::cmp::Reverse;
use std::collections::{BinaryHeap, BTreeMap, HashSet, VecDeque};
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
    #[error("Grid has compatibility violation")]
    CompatibilityViolation,
}

#[derive(Debug, Clone)]
pub struct Grid<T> {
    width: usize,
    height: usize,
    cells: Vec<Option<T>>,
    possibilities: Vec<HashSet<T>>,
    compatibility: CompatibilityMap<T>,
    propagation_queue: VecDeque<(usize, usize)>,
    entropy_queue: BinaryHeap<Reverse<(usize, usize, usize)>>,
}

impl<T> Grid<T>
where
    T: Tile<T>,
{
    pub fn new(width: usize, height: usize, compatibility: CompatibilityMap<T>) -> Self {
        let possibilities: Vec<HashSet<T>> = vec![T::all().into_iter().collect(); width * height];
        let mut entropy_queue = BinaryHeap::new();
        let mut propagation_queue = VecDeque::new();

        for y in 0..height {
            for x in 0..width {
                let index = y * width + x;
                entropy_queue.push(Reverse((possibilities[index].len(), x, y)));
                if possibilities[index].len() == 1 {
                    propagation_queue.push_back((x, y));
                }
            }
        }

        Self {
            width,
            height,
            cells: vec![None; width * height],
            compatibility,
            possibilities,
            propagation_queue,
            entropy_queue,
        }
    }

    pub fn get(&self, x: usize, y: usize) -> Option<T> {
        if x < self.width && y < self.height {
            self.cells[self.xy_to_index(x, y)]
        } else {
            None
        }
    }

    pub fn set(&mut self, x: usize, y: usize, tile: T) {
        if x < self.width && y < self.height {
            let index = self.xy_to_index(x, y);
            self.cells[index] = Some(tile);
        }
    }

    #[inline]
    pub fn xy_to_index(&self, x: usize, y: usize) -> usize {
        y * self.width + x
    }

    #[inline]
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

    pub fn propagate_constraints(&mut self, polygon: Polygon) {
        while let Some((x, y)) = self.propagation_queue.pop_front() {
            let Some(tile) = self.get(x, y) else {
                continue;
            };
            for (side, neighbor_index_opt) in self.neighbor_indexes(x, y, polygon) {
                if let Some(neighbor_index) = neighbor_index_opt {
                    if let Some(allowed_tiles) = self.compatibility.get(tile, side).unwrap() {
                        let possibilities = &mut self.possibilities[neighbor_index];
                        let old_len = possibilities.len();
                        possibilities.retain(|t| allowed_tiles.contains(t));
                        if possibilities.len() < old_len {
                            self.propagation_queue
                                .push_back(self.index_to_xy(neighbor_index));
                            self.update_entropy(x, y);
                        }
                    }
                }
            }
        }
    }

    pub fn update_entropy(&mut self, x: usize, y: usize) {
        let index = self.xy_to_index(x, y);
        let entropy = self.possibilities[index].len();
        self.entropy_queue.push(Reverse((entropy, x, y)));
    }

    pub fn is_valid(&self, allow_none: bool, polygon: Polygon) -> Result<bool, GridError> {
        for (ix, cell) in self.cells.iter().enumerate() {
            let Some(&tile) = cell.as_ref() else {
                if allow_none {
                    continue;
                } else {
                    return Ok(false);
                }
            };
            let (x, y) = self.index_to_xy(ix);
            for (side, nix) in self.neighbor_indexes(x, y, polygon) {
                let Some(nix) = nix else {
                    continue;
                };
                let Some(neighbor_tile) = self.get_by_index(nix) else {
                    // could be out of bounds, so we skip checking for none
                    // if this was in bounds and none, it will still be caught eventually in the loop
                    continue;
                };
                let Some(compatible) = self.compatibility.get(tile, side)? else {
                    unreachable!("bad compatibility map?")
                };
                if !compatible.contains(&neighbor_tile) {
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    pub fn collapse_and_validate(&mut self, polygon: Polygon) -> Result<bool, GridError> {
        let res = self.collapse(polygon);
        if !self.is_valid(true, polygon)? {
            return Err(GridError::CompatibilityViolation);
        }
        return res;
    }

    pub fn collapse(&mut self, polygon: Polygon) -> Result<bool, GridError> {
        let mut rng = thread_rng();

        // Re-load possibilities each iteration to account for external changes
        for (ix, cell) in self.cells.iter().enumerate() {
            let Some(&tile) = cell.as_ref() else {
                continue;
            };
            self.possibilities[ix] = HashSet::from([tile]);
            let (x, y) = self.index_to_xy(ix);
            for (side, nix) in self.neighbor_indexes(x, y, polygon) {
                let Some(nix) = nix else {
                    continue;
                };
                if let Some(compatible) = self.compatibility.get(tile, side)? {
                    self.possibilities[nix].retain(|p| compatible.contains(p))
                } else {
                    unreachable!("bad compatibility map?")
                }
            }
        }

        // Initialize possibilities
        self.entropy_queue.clear();
        self.propagation_queue.clear();

        // Reinitialize the entropy queue and propagation queue
        for y in 0..self.height {
            for x in 0..self.width {
                let index = y * self.width + x;
                self.update_entropy(x, y);
                if self.possibilities[index].len() == 1 {
                    self.propagation_queue.push_back((x, y));
                }
            }
        }

        // Propagate constraints for initially determined tiles
        self.propagate_constraints(polygon);

        while let Some(Reverse((_, x, y))) = self.entropy_queue.pop() {
            let index = y * self.width + x;
            if self.possibilities[index].len() > 1 {
                if let Some(&tile) = self.possibilities[index]
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .choose(&mut rng)
                {
                    self.set(x, y, tile);
                    // set the possibilities to the tile we just set it as
                    self.possibilities[index] = HashSet::from([tile]);

                    self.propagation_queue.push_back((x, y));
                    self.propagate_constraints(polygon);

                    // Update entropy for neighbors
                    for (_, neighbor_index_opt) in self.neighbor_indexes(x, y, polygon) {
                        if let Some(neighbor_index) = neighbor_index_opt {
                            let (nx, ny) = self.index_to_xy(neighbor_index);
                            self.update_entropy(nx, ny);
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

        assert_eq!(grid.is_valid(false, Polygon::Square), Ok(true));

        Ok(())
    }

    #[test]
    fn test_is_valid() -> anyhow::Result<()> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
        enum MyTile {
            A,
            B,
        }
        impl MyTile {
            fn compatible(&self) -> Vec<Self> {
                match self {
                    MyTile::A => vec![MyTile::A],
                    MyTile::B => vec![],
                }
            }
        }
        impl Display for MyTile {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let str = match self {
                    MyTile::A => "A",
                    MyTile::B => "B",
                };
                write!(f, "{str}")
            }
        }

        impl TileInstance for MyTile {}

        impl Tile<Self> for MyTile {
            fn all() -> Vec<Self> {
                vec![MyTile::A, MyTile::B]
            }
        }

        let mut compatibility = CompatibilityMap::new(Polygon::Square);

        for tile in MyTile::all() {
            for side in [Side::Top, Side::Right, Side::Bottom, Side::Left] {
                compatibility.add(tile, side, tile.compatible())?;
            }
        }

        let mut grid = Grid::new(2, 1, compatibility);

        // don't allow empty cells
        assert_eq!(grid.is_valid(false, Polygon::Square), Ok(false));
        // allow empty cells
        assert_eq!(grid.is_valid(true, Polygon::Square), Ok(true));

        grid.set(0, 0, MyTile::A);
        grid.set(1, 0, MyTile::B);

        // dont allow bad compat
        assert_eq!(grid.is_valid(false, Polygon::Square), Ok(false));

        grid.set(1, 0, MyTile::A);

        // valid compat
        assert_eq!(grid.is_valid(false, Polygon::Square), Ok(true));

        Ok(())
    }
}
