use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::mem;

use rand::prelude::*;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{Side, Tile};
use crate::compatibility_map::{CompatibilityMap, CompatibilityMapError};
use crate::grid::{Grid, GridError, GridType};

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
    #[error("{0:?}")]
    CompatibilityMapError(#[from] CompatibilityMapError<GT, T>),
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
struct WfcState<T>
where
    T: Tile<T>,
{
    set_index: usize,
    possibilities: Vec<HashSet<T>>,
    propagation_queue: VecDeque<(usize, usize)>,
    entropy_queue: BinaryHeap<Reverse<(usize, usize, usize)>>,
}

#[derive(Default, Serialize, Deserialize)]
pub struct WaveFunctionCollapse<GT, T>
where
    GT: ?Sized + GridType<T>,
    T: Tile<T>,
{
    grid: Grid<GT, T>,
    wrap_mode: Option<WrapMode>,
    state_history: Vec<WfcState<T>>,
    invalid_possibilities: Vec<HashSet<T>>,
    possibilities: Vec<HashSet<T>>,
    compatibility: CompatibilityMap<GT, T>,
    propagation_queue: VecDeque<(usize, usize)>,
    entropy_queue: BinaryHeap<Reverse<(usize, usize, usize)>>,
}

impl<GT, T> WaveFunctionCollapse<GT, T>
where
    GT: ?Sized + GridType<T> + Serialize + for<'a> Deserialize<'a> + Default,
    T: Tile<T> + Serialize + for<'a> Deserialize<'a> + Default,
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
            state_history: vec![],
            wrap_mode: None,
            invalid_possibilities: possibilities.iter().map(|_| HashSet::new()).collect(),
            possibilities,
            compatibility,
            propagation_queue,
            entropy_queue,
        }
    }

    pub fn set_wrap_mode(&mut self, wrap_mode: Option<WrapMode>) {
        self.wrap_mode = wrap_mode;
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

    pub fn cached_invalid_possibilities(&self) -> &Vec<HashSet<T>> {
        &self.invalid_possibilities
    }

    fn save_state(&mut self, index: usize) {
        self.state_history.push(WfcState {
            set_index: index,
            possibilities: self.possibilities.clone(),
            propagation_queue: self.propagation_queue.clone(),
            entropy_queue: self.entropy_queue.clone(),
        })
    }

    fn pop_state(&mut self) {
        let Some(last_state) = self.state_history.pop() else {
            panic!("exhausted state history");
        };
        let (x, y) = self.grid.index_to_xy(last_state.set_index);
        self.grid.unset(x, y);
        let _ = mem::replace(&mut self.possibilities, last_state.possibilities);
        let _ = mem::replace(&mut self.propagation_queue, last_state.propagation_queue);
        let _ = mem::replace(&mut self.entropy_queue, last_state.entropy_queue);
    }

    fn propagate_constraints(&mut self) {
        while let Some((x, y)) = self.propagation_queue.pop_front() {
            for (side, neighbor_index_opt) in self.grid.neighbor_indexes(x, y, self.wrap_mode) {
                if let Some(neighbor_index) = neighbor_index_opt {
                    let old_len = if self.invalid_possibilities[neighbor_index].is_empty() {
                        self.possibilities[neighbor_index].len()
                    } else {
                        self.possibilities[neighbor_index]
                            .difference(&self.invalid_possibilities[neighbor_index])
                            .count()
                    };
                    let possibilities = &mut self.possibilities[neighbor_index];
                    possibilities.retain(|t| {
                        let passed_allowed_check = if let Some(tile) = self.grid.get(x, y) {
                            if let Some(allowed_tiles) = self.compatibility.get(tile, side) {
                                allowed_tiles.contains(t)
                            } else {
                                unreachable!("bad compat map?");
                            }
                        } else {
                            // default true
                            true
                        };
                        passed_allowed_check
                            && !self.invalid_possibilities[neighbor_index].contains(t)
                    });
                    if possibilities.len() < old_len {
                        let (nx, ny) = self.grid.index_to_xy(neighbor_index);
                        self.propagation_queue.push_back((nx, ny));
                        self.update_entropy(nx, ny);
                    }
                }
            }
        }
    }

    fn update_entropy(&mut self, x: usize, y: usize) {
        let index = self.grid.xy_to_index(x, y);
        let entropy = if self.invalid_possibilities[index].is_empty() {
            self.possibilities[index].len()
        } else {
            self.possibilities[index]
                .difference(&self.invalid_possibilities[index])
                .count()
        };
        self.entropy_queue.push(Reverse((entropy, x, y)));
    }

    fn _step(&mut self, rng: &mut impl Rng) -> Option<(Option<T>, (usize, usize))> {
        enum State<T> {
            SetAny(Vec<T>),
            Backtrack,
        }
        while let Some(Reverse((_, x, y))) = self.entropy_queue.pop() {
            let index = self.grid.xy_to_index(x, y);
            let state = if self.possibilities[index].is_empty() {
                State::Backtrack
            } else {
                let mut choices = self.possibilities[index]
                    .difference(&self.invalid_possibilities[index])
                    .collect::<Vec<_>>();
                // sort required for deterministic generation
                choices.sort();
                if choices.is_empty() {
                    State::Backtrack
                } else {
                    State::SetAny(choices)
                }
            };
            let mut was_set = false;
            let mut skip = false;
            let res = match state {
                State::SetAny(choices) => {
                    was_set = true;
                    if self.grid.get(x, y).is_some() {
                        // TODO: we should prevent redundant entries being inserted into binary heap
                        // this would likely be a redundant set so we flag to skip if the grid is not filled
                        skip = true;
                        (None, (x, y))
                    } else {
                        // > 0.0 is used to make sure if that's the only available tile it's still allowed to be selected
                        const MIN_WEIGHT: f64 = 0.001;
                        let total_cells = self.grid.size();
                        let total_set_cells = self.grid.set_count();
                        let expected_distributions = if let Some(distributions) = T::distribution()
                        {
                            let sum = distributions.values().sum::<f64>();
                            Some(
                                distributions
                                    .into_iter()
                                    .map(|(k, v)| (k, v / sum))
                                    .collect::<HashMap<_, _>>(),
                            )
                        } else {
                            None
                        };

                        let Ok(&tile) = choices
                            .into_iter()
                            .collect::<Vec<_>>()
                            .choose_weighted(rng, |&t| {
                                if let Some(expected_distributions) = &expected_distributions {
                                    let target_distribution_map = self
                                        .grid
                                        .set_count_map()
                                        .iter()
                                        .map(|(k, &current_count)| {
                                            let current_count = current_count as f64;
                                            let expected_distribution = *expected_distributions
                                                .get(k)
                                                .unwrap_or_else(|| unreachable!());

                                            let total = if total_set_cells == 0 {
                                                // to prevent using 0 on the first iteration we use the total cells
                                                total_cells
                                            } else {
                                                // otherwise we use the total set so the distribution can be adaptive
                                                total_set_cells
                                            };

                                            let expected_count =
                                                total as f64 * expected_distribution;
                                            let weight = if expected_count == 0.0 {
                                                // prevent divide by zero
                                                MIN_WEIGHT
                                            } else {
                                                // 0.0 - 1.0
                                                let current_ratio = current_count / expected_count;
                                                // subtract 1.0 to identify the distribution deficit for 0.0 - 1.0.
                                                // Higher deficit = higher weight.
                                                let deficit = 1.0 - current_ratio;
                                                // Ensure weight is not negative
                                                deficit.max(MIN_WEIGHT)
                                            };
                                            (k, weight)
                                        })
                                        .collect::<HashMap<_, _>>();
                                    target_distribution_map.get(t).cloned().unwrap_or_default()
                                } else {
                                    T::probability().get(t).cloned().unwrap_or_default()
                                }
                            })
                            .cloned()
                        else {
                            unreachable!()
                        };
                        self.grid.set(x, y, tile);
                        // set the possibilities to the tile we just set it as
                        self.possibilities[index] = HashSet::from([tile]);

                        self.propagation_queue.push_back((x, y));
                        self.propagate_constraints();
                        self.save_state(index);
                        (Some(tile), (x, y))
                    }
                }
                State::Backtrack => {
                    println!("failed to set ({x},{y}) - backtracking");
                    self.pop_state();
                    (None, (x, y))
                }
            };
            // if filled and last entry was a set, then we clear the queues before returning the result
            if self.grid.is_filled() && was_set {
                self.propagation_queue.clear();
                self.entropy_queue.clear();
            }
            // otherwise if we flagged to skip then we continue processing the next item in the queue instead of returning a result
            else if skip {
                continue;
            }
            return Some(res);
        }
        None
    }

    pub fn step(&mut self) -> Option<(Option<T>, (usize, usize))> {
        self._step(&mut thread_rng())
    }

    pub fn step_with_custom_rng(
        &mut self,
        rng: &mut impl Rng,
    ) -> Option<(Option<T>, (usize, usize))> {
        self._step(rng)
    }

    pub fn initialize_collapse(&mut self) {
        println!("checking external or previous collapse changes");
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

        println!("initializing queues");
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

        println!("propagating");
        // Propagate constraints for initially determined tiles
        self.propagate_constraints();
    }

    fn _perform_all_steps(&mut self, rng: &mut impl Rng) {
        println!("collapsing");
        while self._step(rng).is_some() {}
    }

    pub fn perform_all_steps(&mut self) {
        self._perform_all_steps(&mut thread_rng())
    }

    pub fn perform_all_steps_with_custom_rng(&mut self, rng: &mut impl Rng) {
        println!("collapsing");
        while self.step_with_custom_rng(rng).is_some() {}
    }

    fn _collapse(&mut self, rng: &mut impl Rng) -> bool {
        self.initialize_collapse();

        self._perform_all_steps(rng);

        // Check if any cells are still None
        self.is_all_filled()
    }

    pub fn collapse(&mut self) -> bool {
        self._collapse(&mut thread_rng())
    }

    pub fn collapse_with_custom_rng(&mut self, rng: &mut impl Rng) -> bool {
        self._collapse(rng)
    }

    fn _collapse_and_validate(
        &mut self,
        rng: &mut impl Rng,
    ) -> Result<bool, WaveFunctionCollapseError<GT, T>> {
        let res = self.collapse();
        if !self.is_valid(true) {
            Err(GridError::CompatibilityViolation.into())
        } else {
            Ok(res)
        }
    }

    pub fn collapse_and_validate(&mut self) -> Result<bool, WaveFunctionCollapseError<GT, T>> {
        self._collapse_and_validate(&mut thread_rng())
    }

    pub fn collapse_and_validate_with_custom_rng(
        &mut self,
        rng: &mut impl Rng,
    ) -> Result<bool, WaveFunctionCollapseError<GT, T>> {
        self._collapse_and_validate(rng)
    }

    pub fn is_all_filled(&self) -> bool {
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
