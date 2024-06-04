use bevy::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    Default,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd,
    Reflect,
    Serialize,
    Deserialize,
)]
pub enum HexSegmentId {
    #[default]
    None,
    Grass,
    Mountain,
    MountainPeak,
    River,
    // TODO: too many permutations
    // Creek,
    Ocean,
    Sand,
}

impl HexSegmentId {
    pub fn all() -> Vec<HexSegmentId> {
        vec![
            Self::Grass,
            Self::Mountain,
            Self::MountainPeak,
            Self::River,
            // Self::Creek,
            Self::Ocean,
            Self::Sand,
        ]
    }
    pub fn compatible(&self) -> Vec<HexSegmentId> {
        match self {
            Self::None => vec![],
            Self::Grass => vec![
                Self::Grass,
                Self::Sand,
                Self::Mountain,
                //Self::Creek,
            ],
            Self::Mountain => vec![
                Self::Mountain,
                Self::MountainPeak,
                Self::Grass, /*Self::Creek*/
            ],
            Self::MountainPeak => vec![Self::Mountain /*Self::Creek*/],
            Self::River => vec![
                Self::River,
                Self::Ocean,
                //Self::Creek,
                Self::Sand,
            ],
            // Self::Creek => vec![
            //     Self::Creek,
            //     Self::River,
            //     Self::Ocean,
            //     Self::MountainPeak,
            //     Self::Grass,
            //     Self::Mountain,
            //     Self::Sand,
            // ],
            Self::Ocean => vec![Self::Ocean, Self::River /*Self::Creek*/],
            Self::Sand => vec![
                Self::Sand,
                Self::Grass,
                Self::River,
                // Self::Creek,
            ],
        }
    }
    pub fn probability(&self) -> f64 {
        match self {
            HexSegmentId::None => 0.0,
            HexSegmentId::Grass => 0.75,
            HexSegmentId::Mountain => 0.2,
            HexSegmentId::MountainPeak => 0.5,
            HexSegmentId::River => 0.3,
            HexSegmentId::Ocean => 0.75,
            HexSegmentId::Sand => 0.25,
        }
    }
    pub fn as_color(&self) -> Color {
        match self {
            Self::None => Color::BLACK,
            HexSegmentId::Grass => Color::DARK_GREEN,
            HexSegmentId::Mountain => Color::GRAY,
            HexSegmentId::MountainPeak => Color::WHITE,
            HexSegmentId::River => Color::BLUE,
            // HexSegmentId::Creek => Color::CYAN,
            HexSegmentId::Ocean => Color::MIDNIGHT_BLUE,
            HexSegmentId::Sand => Color::rgb(0.82, 0.70, 0.55), // Tan
        }
    }
}
