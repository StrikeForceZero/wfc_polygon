use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod compatibility_map;
pub mod grid;
pub mod wfc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum HexagonType {
    FlatTop,
    PointyTop,
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize,
)]
pub struct Triangle;

impl From<Triangle> for Polygon {
    fn from(_: Triangle) -> Self {
        Polygon::Triangle
    }
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize,
)]
pub struct Square;

impl From<Square> for Polygon {
    fn from(_: Square) -> Self {
        Polygon::Square
    }
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize,
)]
pub struct FlatTopHexagon;

impl From<FlatTopHexagon> for Polygon {
    fn from(_: FlatTopHexagon) -> Self {
        Polygon::Hexagon(HexagonType::FlatTop)
    }
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize,
)]
pub struct PointyTopHexagon;

impl From<PointyTopHexagon> for Polygon {
    fn from(_: PointyTopHexagon) -> Self {
        Polygon::Hexagon(HexagonType::PointyTop)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
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

impl Side {
    pub fn opposite(&self) -> Self {
        match self {
            Self::Top => Self::Bottom,
            Self::Bottom => Self::Top,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
            Self::TopLeft => Self::BottomRight,
            Self::TopRight => Self::BottomLeft,
            Self::BottomLeft => Self::TopRight,
            Self::BottomRight => Self::TopLeft,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum TriangleSide {
    TopRight,
    Bottom,
    TopLeft,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum SquareSide {
    Top,
    Right,
    Bottom,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum FlatTopHexSide {
    Top,
    TopRight,
    BottomRight,
    Bottom,
    BottomLeft,
    TopLeft,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum PointyTopHexSide {
    TopRight,
    Right,
    BottomRight,
    BottomLeft,
    Left,
    TopLeft,
}

#[derive(Debug, Copy, Clone, Error)]
pub enum TryFromSideError {
    #[error("invalid side {1:?} for {0:?}")]
    InvalidSide(Polygon, Side),
}

impl TryFrom<Side> for TriangleSide {
    type Error = TryFromSideError;

    fn try_from(value: Side) -> Result<Self, Self::Error> {
        Ok(match value {
            Side::Bottom => Self::Bottom,
            Side::TopLeft => Self::TopLeft,
            Side::TopRight => Self::TopRight,
            _ => return Err(TryFromSideError::InvalidSide(Triangle.into(), value)),
        })
    }
}

impl From<TriangleSide> for Side {
    fn from(value: TriangleSide) -> Self {
        match value {
            TriangleSide::Bottom => Self::Bottom,
            TriangleSide::TopLeft => Self::TopLeft,
            TriangleSide::TopRight => Self::TopRight,
        }
    }
}

impl TryFrom<Side> for SquareSide {
    type Error = TryFromSideError;

    fn try_from(value: Side) -> Result<Self, Self::Error> {
        Ok(match value {
            Side::Top => Self::Top,
            Side::Right => Self::Right,
            Side::Bottom => Self::Bottom,
            Side::Left => Self::Left,
            _ => return Err(TryFromSideError::InvalidSide(Square.into(), value)),
        })
    }
}

impl From<SquareSide> for Side {
    fn from(value: SquareSide) -> Self {
        match value {
            SquareSide::Top => Self::Top,
            SquareSide::Right => Self::Right,
            SquareSide::Bottom => Self::Bottom,
            SquareSide::Left => Self::Left,
        }
    }
}

impl TryFrom<Side> for FlatTopHexSide {
    type Error = TryFromSideError;

    fn try_from(value: Side) -> Result<Self, Self::Error> {
        Ok(match value {
            Side::Top => Self::Top,
            Side::TopRight => Self::TopRight,
            Side::BottomRight => Self::BottomRight,
            Side::Bottom => Self::Bottom,
            Side::BottomLeft => Self::BottomLeft,
            Side::TopLeft => Self::TopLeft,
            _ => return Err(TryFromSideError::InvalidSide(FlatTopHexagon.into(), value)),
        })
    }
}

impl From<FlatTopHexSide> for Side {
    fn from(value: FlatTopHexSide) -> Self {
        match value {
            FlatTopHexSide::Top => Self::Top,
            FlatTopHexSide::TopRight => Self::TopRight,
            FlatTopHexSide::BottomRight => Self::BottomRight,
            FlatTopHexSide::Bottom => Self::Bottom,
            FlatTopHexSide::BottomLeft => Self::BottomLeft,
            FlatTopHexSide::TopLeft => Self::TopLeft,
        }
    }
}

impl TryFrom<Side> for PointyTopHexSide {
    type Error = TryFromSideError;

    fn try_from(value: Side) -> Result<Self, Self::Error> {
        Ok(match value {
            Side::TopRight => Self::TopRight,
            Side::Right => Self::Right,
            Side::BottomRight => Self::BottomRight,
            Side::BottomLeft => Self::BottomLeft,
            Side::Left => Self::Left,
            Side::TopLeft => Self::TopLeft,
            _ => {
                return Err(TryFromSideError::InvalidSide(
                    PointyTopHexagon.into(),
                    value,
                ))
            }
        })
    }
}

impl From<PointyTopHexSide> for Side {
    fn from(value: PointyTopHexSide) -> Self {
        match value {
            PointyTopHexSide::TopRight => Self::TopRight,
            PointyTopHexSide::Right => Self::Right,
            PointyTopHexSide::BottomRight => Self::BottomRight,
            PointyTopHexSide::BottomLeft => Self::BottomLeft,
            PointyTopHexSide::Left => Self::Left,
            PointyTopHexSide::TopLeft => Self::TopLeft,
        }
    }
}

pub trait TileInstance: Debug + Clone + Copy + PartialEq + PartialOrd + Ord + Hash {}

pub trait Tile<T: TileInstance>: TileInstance {
    fn all() -> Vec<T>;
    fn probability() -> HashMap<T, f64> {
        Self::all().into_iter().map(|t| (t, 1.0)).collect()
    }
}
