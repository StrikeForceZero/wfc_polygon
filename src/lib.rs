use std::fmt::{Debug, Display};
use std::hash::Hash;

use rand::seq::SliceRandom;

pub mod compatibility_map;
pub mod grid;

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

pub trait TileInstance: Debug + Clone + Copy + PartialEq + PartialOrd + Ord + Hash {}

pub trait Tile<T: TileInstance>: TileInstance {
    fn all() -> Vec<T>;
}
