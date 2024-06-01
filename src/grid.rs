use std::fmt::{Display, Formatter};

use thiserror::Error;

use crate::{HexagonType, Polygon, Side, Tile};
use crate::compatibility_map::CompatibilityMapError;

macro_rules! cast_tuple {
    ($from:ty, $to:ty, $tuple:expr) => {{
        let tuple = $tuple;
        match <[$from; 2]>::from(tuple).map(<$to as TryFrom<_>>::try_from) {
            [Ok(a), Ok(b)] => (a, b),
            [Err(err), _] | [_, Err(err)] => {
                let (a, b) = tuple;
                panic!("{a} {b} {err}");
            }
        }
    }};
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
    polygon: Polygon,
    width: usize,
    height: usize,
    cells: Vec<Option<T>>,
}

impl<T> Grid<T>
where
    T: Tile<T>,
{
    pub fn new(polygon: Polygon, width: usize, height: usize) -> Self {
        Self {
            polygon,
            width,
            height,
            cells: vec![None; width * height],
        }
    }

    pub fn polygon(&self) -> Polygon {
        self.polygon
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn cells(&self) -> &[Option<T>] {
        &self.cells
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

    pub fn get_neighbor_index(&self, x: usize, y: usize, side: Side) -> Option<usize> {
        let x = x as i32;
        let y = y as i32;
        let width = self.width as i32;
        let height = self.height as i32;
        let polygon = self.polygon;
        let nix = match polygon {
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
        // Ensure neighbor_index is within valid range
        if nix >= 0 && nix < (width * height) {
            let (nx, ny) = cast_tuple!(usize, i32, self.index_to_xy(nix as usize));

            if nx >= 0
                && nx < width
                && ny >= 0
                && ny < height
                // make sure we didn't wrap around the grid
                && x.abs_diff(nx) <= 1
                && y.abs_diff(ny) <= 1
            {
                Some(nix as usize)
            } else {
                // overflow
                None
            }
        } else {
            // negative
            None
        }
    }

    pub fn neighbor_indexes(&self, x: usize, y: usize) -> Vec<(Side, Option<usize>)> {
        match self.polygon {
            Polygon::Triangle => [Side::TopLeft, Side::TopRight, Side::Bottom]
                .map(|side| (side, self.get_neighbor_index(x, y, side)))
                .to_vec(),
            Polygon::Square => [Side::Top, Side::Right, Side::Bottom, Side::Left]
                .map(|side| (side, self.get_neighbor_index(x, y, side)))
                .to_vec(),
            Polygon::Hexagon(HexagonType::FlatTop) => [
                Side::Top,
                Side::TopRight,
                Side::TopLeft,
                Side::BottomRight,
                Side::BottomLeft,
                Side::Bottom,
            ]
            .map(|side| (side, self.get_neighbor_index(x, y, side)))
            .to_vec(),
            Polygon::Hexagon(HexagonType::PointyTop) => [
                Side::TopLeft,
                Side::TopRight,
                Side::Left,
                Side::Right,
                Side::BottomLeft,
                Side::BottomRight,
            ]
            .map(|side| (side, self.get_neighbor_index(x, y, side)))
            .to_vec(),
        }
    }

    pub fn neighbors(&self, x: usize, y: usize) -> Vec<(Side, Option<T>)> {
        self.neighbor_indexes(x, y)
            .into_iter()
            .map(|(side, index)| (side, index.and_then(|index| self.get_by_index(index))))
            .collect()
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
    T: Tile<T> + Display,
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
    use crate::compatibility_map::CompatibilityMap;
    use crate::TileInstance;
    use crate::wfc::WaveFunctionCollapse;

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

        let mut wfc = WaveFunctionCollapse::new_with_compatibility(
            Grid::new(Polygon::Square, 10, 10),
            compatibility,
        );
        let max_retries = 10;

        for _ in 1..=max_retries {
            if wfc.collapse()? {
                break;
            }
        }

        assert_eq!(wfc.is_valid(false), Ok(true));

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

        let mut wfc = WaveFunctionCollapse::new_with_compatibility(
            Grid::new(Polygon::Square, 2, 1),
            compatibility,
        );

        // don't allow empty cells
        assert_eq!(wfc.is_valid(false), Ok(false));
        // allow empty cells
        assert_eq!(wfc.is_valid(true), Ok(true));

        wfc.grid_mut().set(0, 0, MyTile::A);
        wfc.grid_mut().set(1, 0, MyTile::B);

        // dont allow bad compat
        assert_eq!(wfc.is_valid(false), Ok(false));

        wfc.grid_mut().set(1, 0, MyTile::A);

        // valid compat
        assert_eq!(wfc.is_valid(false), Ok(true));

        Ok(())
    }
}
