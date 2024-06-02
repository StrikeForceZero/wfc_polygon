use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet, VecDeque};
use std::fmt::Debug;
use std::num::NonZeroUsize;
use std::path::PathBuf;

use rand::prelude::*;
use rand::thread_rng;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::compatibility_map::CompatibilityMap;
use crate::grid::{Grid, GridError, GridType};
use crate::state_stack::LazyStateStack;
use crate::{Side, Tile};

#[derive(Serialize, Deserialize)]
struct SavedState<GT, T>(WaveFunctionCollapse<GT, T>)
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>;

struct StateHistory {
    state_stack: LazyStateStack<PathBuf>,
}

impl StateHistory {
    fn new() -> Self {
        Self {
            state_stack: LazyStateStack::new(
                PathBuf::from("wfc_state_history.bin"),
                // TODO: no idea what what this number should be set to
                NonZeroUsize::new(512).unwrap(),
            ),
        }
    }
}

#[derive(Debug, Error)]
pub enum WaveFunctionCollapseError<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    #[error("Failed to read/write from state history: {0:?}")]
    StateStackError(#[from] std::io::Error),
    #[error("No more states available to backtrack")]
    StateStackExhausted,
    #[error("{0:?}")]
    GridError(#[from] GridError<GT, T>),
    #[error("Failed to serialize/deserialize state history: {0:?}")]
    SerializationError(#[from] bincode::Error),
}

#[derive(Serialize, Deserialize)]
pub struct WaveFunctionCollapse<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    grid: Grid<GT, T>,
    possibilities: Vec<HashSet<T>>,
    compatibility: CompatibilityMap<GT, T>,
    propagation_queue: VecDeque<(usize, usize)>,
    entropy_queue: BinaryHeap<Reverse<(usize, usize, usize)>>,
    last_set_index: Option<usize>,
    #[serde(skip_serializing, skip_deserializing)]
    state_history: Option<StateHistory>,
}

impl<GT, T> WaveFunctionCollapse<GT, T>
where
    GT: ?Sized + GridType<T> + Serialize + for<'a> Deserialize<'a>,
    T: Tile<T> + Serialize + for<'a> Deserialize<'a>,
    <<GT as GridType<T>>::SideType as TryFrom<Side>>::Error: Debug,
{
    pub fn new(grid: Grid<GT, T>) -> Self {
        let compatibility = CompatibilityMap::new();
        Self::new_with_compatibility(grid, compatibility)
    }
    pub fn new_with_compatibility(
        grid: Grid<GT, T>,
        compatibility: CompatibilityMap<GT, T>,
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
            possibilities,
            compatibility,
            propagation_queue,
            entropy_queue,
            last_set_index: None,
            state_history: Some(StateHistory::new()),
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
            for (side, neighbor_index_opt) in self.grid.neighbor_indexes(x, y) {
                if let Some(neighbor_index) = neighbor_index_opt {
                    if let Some(allowed_tiles) = self.compatibility.get(tile, side) {
                        let possibilities = &mut self.possibilities[neighbor_index];
                        let old_len = possibilities.len();
                        possibilities.retain(|t| allowed_tiles.contains(t));
                        if possibilities.len() < old_len {
                            let (nx, ny) = self.grid.index_to_xy(neighbor_index);
                            self.propagation_queue.push_back((nx, ny));
                            self.update_entropy(nx, ny);
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

    fn backtrack(&mut self) -> Result<(), WaveFunctionCollapseError<GT, T>> {
        let Some(prev_state_bin) = self
            .state_history
            .as_mut()
            .expect("expected state history")
            .state_stack
            .pop()
        else {
            return Err(WaveFunctionCollapseError::StateStackExhausted);
        };
        let last_set_index = self
            .last_set_index
            .unwrap_or_else(|| unreachable!("backtrack called before collapse"));
        let last_set_tile =
            self.grid.cells()[last_set_index].expect("expected tile at least set index");
        println!("backtracking {last_set_index} {last_set_tile:?}");
        let (x, y) = self.grid.index_to_xy(last_set_index);
        let prev_state = bincode::deserialize::<Self>(&prev_state_bin)?;
        let state_history = self.state_history.take();
        *self = prev_state;
        self.state_history = state_history;
        self.possibilities[last_set_index].remove(&last_set_tile);

        self.propagation_queue.push_back((x, y));
        self.propagate_constraints();

        Ok(())
    }

    pub fn collapse(&mut self) -> Result<bool, WaveFunctionCollapseError<GT, T>> {
        let mut rng = thread_rng();

        // Re-load possibilities each iteration to account for external changes
        for (ix, cell) in self.grid.cells().iter().enumerate() {
            let Some(&tile) = cell.as_ref() else {
                continue;
            };
            self.possibilities[ix] = HashSet::from([tile]);
            let (x, y) = self.grid.index_to_xy(ix);
            for (side, nix) in self.grid.neighbor_indexes(x, y) {
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
                    self.last_set_index = Some(index);
                    self.grid.set(x, y, tile);
                    // set the possibilities to the tile we just set it as
                    self.possibilities[index] = HashSet::from([tile]);

                    self.propagation_queue.push_back((x, y));
                    self.propagate_constraints();

                    // Update entropy for neighbors
                    for (_, neighbor_index_opt) in self.grid.neighbor_indexes(x, y) {
                        if let Some(neighbor_index) = neighbor_index_opt {
                            let (nx, ny) = self.grid.index_to_xy(neighbor_index);
                            self.update_entropy(nx, ny);
                        }
                    }

                    bincode::serialize(&self).and_then(|bin| {
                        Ok(self
                            .state_history
                            .as_mut()
                            .expect("expected state history")
                            .state_stack
                            .push(&bin)?)
                    })?
                }
            } else if self.possibilities[index].len() == 0 {
                self.backtrack()?;
            }
        }

        // Ensure propagation queue is empty and constraints are fully propagated
        self.propagate_constraints();

        // Check if any cells are still None
        for y in 0..self.grid.height() {
            for x in 0..self.grid.width() {
                let index = y * self.grid.width() + x;
                if self.grid.cells()[index].is_none() {
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    pub fn collapse_and_validate(&mut self) -> Result<bool, WaveFunctionCollapseError<GT, T>> {
        let res = self.collapse()?;
        if !self.is_valid(true) {
            Err(GridError::CompatibilityViolation.into())
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
            for (side, nix) in self.grid.neighbor_indexes(x, y) {
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
            for (side, nix) in self.grid.neighbor_indexes(x, y) {
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
