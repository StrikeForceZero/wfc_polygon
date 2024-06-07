use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet, VecDeque};
use std::fmt::Debug;

use rand::prelude::*;
use rand::thread_rng;

use crate::{Side, Tile};
use crate::compatibility_map::CompatibilityMap;
use crate::grid::{Grid, GridError, GridType};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum WrapMode {
    X,
    Y,
    Both,
}

impl WrapMode {
    pub fn wrap_xy(
        &self,
        width: usize,
        height: usize,
        pos: (i32, i32),
        delta: (i32, i32),
    ) -> (i32, i32) {
        let width = width as i32;
        let height = height as i32;
        let (x, y) = pos;
        let (dx, dy) = delta;
        match self {
            WrapMode::X => ((x + dx + width) % width, y + dy),
            WrapMode::Y => (x + dx, (y + dy + height) % height),
            WrapMode::Both => ((x + dx + width) % width, (y + dy + height) % height),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::WrapMode;

    const TEST_SIZE: usize = 2;

    #[test]
    fn test_wrap_xy_x() {
        assert_eq!(
            WrapMode::X.wrap_xy(TEST_SIZE, TEST_SIZE, (1, 0), (1, 0)),
            (0, 0)
        );
        assert_eq!(
            WrapMode::X.wrap_xy(TEST_SIZE, TEST_SIZE, (1, 0), (2, 0)),
            (1, 0)
        );
        assert_eq!(
            WrapMode::X.wrap_xy(TEST_SIZE, TEST_SIZE, (0, 1), (-1, 0)),
            (1, 1)
        );
    }

    #[test]
    fn test_wrap_xy_y() {
        assert_eq!(
            WrapMode::Y.wrap_xy(TEST_SIZE, TEST_SIZE, (0, 1), (0, 1)),
            (0, 0)
        );
        assert_eq!(
            WrapMode::Y.wrap_xy(TEST_SIZE, TEST_SIZE, (0, 1), (0, 2)),
            (0, 1)
        );
        assert_eq!(
            WrapMode::Y.wrap_xy(TEST_SIZE, TEST_SIZE, (1, 0), (0, -1)),
            (1, 1)
        );
    }

    #[test]
    fn test_wrap_xy_both() {
        assert_eq!(
            WrapMode::Both.wrap_xy(TEST_SIZE, TEST_SIZE, (1, 1), (1, 1)),
            (0, 0)
        );
        assert_eq!(
            WrapMode::Both.wrap_xy(TEST_SIZE, TEST_SIZE, (1, 1), (2, 2)),
            (1, 1)
        );
        assert_eq!(
            WrapMode::Both.wrap_xy(TEST_SIZE, TEST_SIZE, (0, 0), (-1, -1)),
            (1, 1)
        );
    }
}

#[derive(Clone)]
pub struct WaveFunctionCollapse<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    grid: Grid<GT, T>,
    wrap_mode: Option<WrapMode>,
    possibilities: Vec<HashSet<T>>,
    compatibility: CompatibilityMap<GT, T>,
    propagation_queue: VecDeque<(usize, usize)>,
    entropy_queue: BinaryHeap<Reverse<(usize, usize, usize)>>,
}

impl<GT, T> WaveFunctionCollapse<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
    <<GT as GridType<T>>::SideType as TryFrom<Side>>::Error: Debug,
{
    pub fn new(grid: Grid<GT, T>, wrap_mode: Option<WrapMode>) -> Self {
        let compatibility = CompatibilityMap::new();
        Self::new_with_compatibility(grid, compatibility, wrap_mode)
    }
    pub fn new_with_compatibility(
        grid: Grid<GT, T>,
        compatibility: CompatibilityMap<GT, T>,
        wrap_mode: Option<WrapMode>,
    ) -> Self {
        let width = grid.width();
        let height = grid.height();
        let possibilities: Vec<HashSet<T>> = vec![T::all().into_iter().collect(); width * height];
        let mut entropy_queue = BinaryHeap::new();
        let mut propagation_queue = VecDeque::new();

        for y in 0..height {
            for x in 0..width {
                let index = grid.xy_to_index(x, y);
                entropy_queue.push(Reverse((possibilities[index].len(), x, y)));
                if possibilities[index].len() == 1 {
                    propagation_queue.push_back((x, y));
                }
            }
        }

        Self {
            grid,
            wrap_mode,
            possibilities,
            compatibility,
            propagation_queue,
            entropy_queue,
        }
    }

    pub fn grid(&self) -> &Grid<GT, T> {
        &self.grid
    }

    pub fn grid_mut(&mut self) -> &mut Grid<GT, T> {
        &mut self.grid
    }

    pub fn cached_possibilities(&self) -> &Vec<HashSet<T>> {
        &self.possibilities
    }

    fn propagate_constraints(&mut self) {
        while let Some((x, y)) = self.propagation_queue.pop_front() {
            let Some(tile) = self.grid.get(x, y) else {
                continue;
            };
            for (side, neighbor_index_opt) in self.grid.neighbor_indexes(x, y, self.wrap_mode) {
                if let Some(neighbor_index) = neighbor_index_opt {
                    if let Some(allowed_tiles) = self.compatibility.get(tile, side) {
                        let possibilities = &mut self.possibilities[neighbor_index];
                        let old_len = possibilities.len();
                        possibilities.retain(|t| allowed_tiles.contains(t));
                        if possibilities.len() < old_len {
                            self.propagation_queue
                                .push_back(self.grid.index_to_xy(neighbor_index));
                            self.update_entropy(x, y);
                        }
                    }
                }
            }
        }
    }

    fn update_entropy(&mut self, x: usize, y: usize) {
        let index = self.grid.xy_to_index(x, y);
        let entropy = self.possibilities[index].len();
        self.entropy_queue.push(Reverse((entropy, x, y)));
    }

    pub fn collapse(&mut self) -> bool {
        let mut rng = thread_rng();

        // Re-load possibilities each iteration to account for external changes
        for (ix, cell) in self.grid.cells().iter().enumerate() {
            let Some(&tile) = cell.as_ref() else {
                continue;
            };
            self.possibilities[ix] = HashSet::from([tile]);
            let (x, y) = self.grid.index_to_xy(ix);
            for (side, nix) in self.grid.neighbor_indexes(x, y, self.wrap_mode) {
                let Some(nix) = nix else {
                    continue;
                };
                if let Some(compatible) = self.compatibility.get(tile, side) {
                    self.possibilities[nix].retain(|p| compatible.contains(p));
                } else {
                    unreachable!("bad compatibility map?")
                }
            }
        }

        // Initialize possibilities
        self.entropy_queue.clear();
        self.propagation_queue.clear();

        // Reinitialize the entropy queue and propagation queue
        for y in 0..self.grid.height() {
            for x in 0..self.grid.width() {
                let index = y * self.grid.width() + x;
                self.update_entropy(x, y);
                if self.possibilities[index].len() == 1 {
                    self.propagation_queue.push_back((x, y));
                }
            }
        }

        // Propagate constraints for initially determined tiles
        self.propagate_constraints();

        while let Some(Reverse((_, x, y))) = self.entropy_queue.pop() {
            let index = self.grid.xy_to_index(x, y);
            if self.possibilities[index].len() > 1
                || self.possibilities[index].len() == 1 && self.grid.get_by_index(index).is_none()
            {
                if self.possibilities[index].len() == 1 && self.grid.get_by_index(index).is_none() {
                    println!("force filling: ({x},{y})")
                }
                if let Some(&tile) = self.possibilities[index]
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .choose(&mut rng)
                {
                    self.grid.set(x, y, tile);
                    // set the possibilities to the tile we just set it as
                    self.possibilities[index] = HashSet::from([tile]);

                    self.propagation_queue.push_back((x, y));
                    self.propagate_constraints();

                    // Update entropy for neighbors
                    for (_, neighbor_index_opt) in self.grid.neighbor_indexes(x, y, self.wrap_mode) {
                        if let Some(neighbor_index) = neighbor_index_opt {
                            let (nx, ny) = self.grid.index_to_xy(neighbor_index);
                            self.update_entropy(nx, ny);
                        }
                    }
                }
            }
        }

        // Ensure propagation queue is empty and constraints are fully propagated
        self.propagate_constraints();

        // Check if any cells are still None
        for y in 0..self.grid.height() {
            for x in 0..self.grid.width() {
                let index = y * self.grid.width() + x;
                if self.grid.cells()[index].is_none() {
                    return false;
                }
            }
        }
        true
    }

    pub fn collapse_and_validate(&mut self) -> Result<bool, GridError<GT, T>> {
        let res = self.collapse();
        if !self.is_valid(true) {
            Err(GridError::CompatibilityViolation)
        } else {
            Ok(res)
        }
    }

    pub fn get_invalids(&self, allow_none: bool) -> HashSet<(usize, usize)> {
        let mut invalids = HashSet::new();
        for (ix, cell) in self.grid.cells().iter().enumerate() {
            let (x, y) = self.grid.index_to_xy(ix);
            let Some(&tile) = cell.as_ref() else {
                if !allow_none {
                    invalids.insert((x, y));
                }
                continue;
            };
            for (side, nix) in self.grid.neighbor_indexes(x, y, self.wrap_mode) {
                let Some(nix) = nix else {
                    continue;
                };
                let Some(neighbor_tile) = self.grid.get_by_index(nix) else {
                    // could be out of bounds, so we skip checking for none
                    // if this was in bounds and none, it will still be caught eventually in the loop
                    continue;
                };
                let Some(compatible) = self.compatibility.get(tile, side) else {
                    unreachable!("bad compatibility map?")
                };
                if !compatible.contains(&neighbor_tile) {
                    println!("ix: {ix},  nix: {nix}, tile: {tile:?}, compatible: {compatible:?}");
                    invalids.insert((x, y));
                }
            }
        }
        invalids
    }

    pub fn is_valid(&self, allow_none: bool) -> bool {
        for (ix, cell) in self.grid.cells().iter().enumerate() {
            let Some(&tile) = cell.as_ref() else {
                if allow_none {
                    continue;
                } else {
                    return false;
                }
            };
            let (x, y) = self.grid.index_to_xy(ix);
            for (side, nix) in self.grid.neighbor_indexes(x, y, self.wrap_mode) {
                let Some(nix) = nix else {
                    continue;
                };
                let Some(neighbor_tile) = self.grid.get_by_index(nix) else {
                    // could be out of bounds, so we skip checking for none
                    // if this was in bounds and none, it will still be caught eventually in the loop
                    continue;
                };
                let Some(compatible) = self.compatibility.get(tile, side) else {
                    unreachable!("bad compatibility map?")
                };
                if !compatible.contains(&neighbor_tile) {
                    return false;
                }
            }
        }
        true
    }
}
