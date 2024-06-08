use bevy::prelude::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, Reflect)]
pub enum HexSegmentId {
    #[default]
    None,
    Grass,
    Mountain,
    MountainPeak,
    River,
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
            Self::Ocean,
            Self::Sand,
        ]
    }
    pub fn compatible(&self) -> Vec<HexSegmentId> {
        match self {
            Self::None => vec![],
            Self::Grass => vec![Self::Grass, Self::Sand, Self::Mountain],
            Self::Mountain => vec![Self::Mountain, Self::MountainPeak, Self::Grass],
            Self::MountainPeak => vec![Self::Mountain],
            Self::River => vec![Self::River, Self::Ocean, Self::Sand],
            Self::Ocean => vec![Self::Ocean, Self::River],
            Self::Sand => vec![Self::Sand, Self::Grass, Self::River],
        }
    }
    pub fn probability(&self) -> f64 {
        match self {
            HexSegmentId::None => 0.0,
            HexSegmentId::Grass => 0.8,
            HexSegmentId::Mountain => 0.25,
            HexSegmentId::MountainPeak => 0.5,
            HexSegmentId::River => 0.6,
            HexSegmentId::Ocean => 0.7,
            HexSegmentId::Sand => 0.35,
        }
    }
    pub fn distribution(&self) -> f64 {
        match self {
            HexSegmentId::None => 0.0,
            HexSegmentId::Grass => 0.8,
            HexSegmentId::Mountain => 0.25,
            HexSegmentId::MountainPeak => 0.01,
            HexSegmentId::River => 0.3,
            HexSegmentId::Ocean => 0.7,
            HexSegmentId::Sand => 0.35,
        }
    }
    pub fn as_color(&self) -> Color {
        match self {
            Self::None => Color::BLACK,
            HexSegmentId::Grass => Color::DARK_GREEN,
            HexSegmentId::Mountain => Color::GRAY,
            HexSegmentId::MountainPeak => Color::WHITE,
            HexSegmentId::River => Color::BLUE,
            HexSegmentId::Ocean => Color::MIDNIGHT_BLUE,
            HexSegmentId::Sand => Color::rgb(0.82, 0.70, 0.55), // Tan
        }
    }
}
