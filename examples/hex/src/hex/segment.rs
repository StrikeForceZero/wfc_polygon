use bevy::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
pub enum HexSegmentId {
    Grass,
    Mountain,
    MountainPeak,
    River,
    Creek,
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
            Self::Creek,
            Self::Ocean,
            Self::Sand,
        ]
    }
    pub fn compatible(&self) -> Vec<HexSegmentId> {
        match self {
            Self::Grass => vec![Self::Grass, Self::Sand, Self::Mountain],
            Self::Mountain => vec![Self::Mountain, Self::MountainPeak, Self::Grass],
            Self::MountainPeak => vec![Self::Mountain, Self::Creek],
            Self::River => vec![Self::River, Self::Creek, Self::Ocean],
            Self::Creek => vec![Self::Creek, Self::River, Self::Ocean, Self::MountainPeak],
            Self::Ocean => vec![Self::Ocean, Self::Creek, Self::River, Self::Sand],
            Self::Sand => vec![Self::Sand, Self::Ocean, Self::Grass],
        }
    }
    pub fn as_color(&self) -> Color {
        match self {
            HexSegmentId::Grass => Color::DARK_GREEN,
            HexSegmentId::Mountain => Color::GRAY,
            HexSegmentId::MountainPeak => Color::WHITE,
            HexSegmentId::River => Color::BLUE,
            HexSegmentId::Creek => Color::CYAN,
            HexSegmentId::Ocean => Color::MIDNIGHT_BLUE,
            HexSegmentId::Sand => Color::rgb(0.82, 0.70, 0.55), // Tan
        }
    }
}